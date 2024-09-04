package org.anon.spareuse.client.analyses

import org.anon.spareuse.client.http.SparriOracleApiClient
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.LookupRequestRepresentation
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSTaintFlowSummaryBuilderImpl, MethodTACProvider}
import org.anon.spareuse.webapi.model.oracle.{ApplicationMethodWithSummaryRepr, LookupResponse, TypeNodeRepr}
import org.opalj.br.{ClassFile, Method}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.NEW
import org.opalj.tac.ComputeTACAIKey
import org.opalj.tac.cg.CFA_1_1_CallGraphKey

import java.io.File
import java.net.URL
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

//TODO: Build a return type / warning
class IFDSTaintFlowAnalysis(classesDirectory: File, pomFile: File) extends ClientAnalysis[Int](classesDirectory, pomFile) {

  private val remoteAnalysisName: String = IFDSTaintFlowSummaryBuilderImpl.analysisName
  private val remoteAnalysisVersion: String = "0.0.1"

  private val oracleApiClient: SparriOracleApiClient = new SparriOracleApiClient

  // (Ab)use the existing taint flow summary builder, which is normally used in the context of an AnalysisRunner instance
  private val taintFlowSummaryBuilder: IFDSTaintFlowSummaryBuilderImpl = new IFDSTaintFlowSummaryBuilderImpl(None)

  override def close(): Unit = {
    super.close()
    oracleApiClient.close()
  }

  override protected[analyses] def requirements: Seq[AnalysisRequirement] =
    getAllDependencies
      .get // Note that any exceptions thrown here will be caught by the calling (final) method ClientAnalysis.checkRequirements()
      .map(dep => AnalysisRequirement(dep.identifier.toGA + "!" + dep.identifier.toString, remoteAnalysisName, remoteAnalysisVersion))
      .toSeq

  override def execute(): Try[Int] = Try {
    val p = getOpalProject(loadJre = false)

    val dependencies = getAllDependencies.get.map(_.identifier.toString)

    val projectTypeMap = p
      .allProjectClassFiles
      .map(cf => (cf.fqn, cf))
      .toMap

    val projectTypeNodes = projectTypeMap
      .values
      .map{ cf => TypeNodeRepr(cf.fqn, cf.superclassType.map(_.fqn), cf.interfaceTypes.map(_.fqn).toSet, cf.isInterfaceDeclaration) }
      .toSet

    val allTypesInitialized = p
      .allProjectClassFiles
      .flatMap(_.methodsWithBody)
      .flatMap(_.body.get.instructions)
      .filter(_.isInstanceOf[NEW])
      .map(_.asNEW.objectType.fqn)
      .toSet

    implicit val provider: MethodTACProvider = p.get(ComputeTACAIKey)

    Try(oracleApiClient.startOracleSession(dependencies, projectTypeNodes, allTypesInitialized, 0, Some("17"))) match {
      case Success(_) =>
        log.info(s"Successfully started resolution session with server, session-id = ${oracleApiClient.getToken.getOrElse("<NONE>")}")

        // Give oracle time to fully initialize
        while(!oracleApiClient.isReadyForInteraction){
          Thread.sleep(100)
        }

        log.info(s"Oracle ready for interaction.")

        // Add all library entry points to a work stack
        val entryPointsToProcess = mutable.Stack.from(getLibraryEntryPoints(p))

        log.info(s"Found a total of ${entryPointsToProcess.size} library entry points.")

        // Handle one entry point after the other
        while(entryPointsToProcess.nonEmpty){
          val currentEntry = entryPointsToProcess.pop()
          Try(oracleApiClient.startResolutionAt(currentEntry.callingContext, currentEntry.ccPC, currentEntry.typesInitialized)).flatten match {
            case Success(_) =>
              log.info(s"Successfully started resolution at entrypoint: ${currentEntry.callingContext.descriptor.toJava(currentEntry.callingContext.name)} , PC=${currentEntry.ccPC}")
              handleOracleInteractionUntilFinished(currentEntry, projectTypeMap)
            case Failure(ex) =>
              log.error(s"Failed to start resolution at entrypoint: ${currentEntry.callingContext.descriptor.toJava(currentEntry.callingContext.name)} , PC=${currentEntry.ccPC}", ex)
          }
        }

        oracleApiClient.finalizeSession() match {
          case Success(_) =>
            log.info(s"Successfully finalized resolution session")
          case Failure(ex) =>
            log.error(s"Failure during session finalization", ex)
        }

        0
      case Failure(ex) =>
        log.error(s"Failed to start oracle session", ex)
        throw ex
    }

  }

  private def getLibraryEntryPoints(project: Project[URL]): Set[EntryPoint] = {
    val cg = project.get(CFA_1_1_CallGraphKey)

    cg
      .reachableMethods()
      .flatMap(ctx => cg.calleesOf(ctx.method).flatMap(t => t._2.map(t2 => (ctx, t._1, t2))))
      .filter{
        case (_, _, callee) =>
          callee.hasContext && !project.isProjectType(callee.method.declaringClassType) && !callee.method.declaringClassType.fqn.startsWith("java")
      }.map{
      case (callerCtx, pc, _) =>
        EntryPoint(callerCtx.method.definedMethod, pc, Set.empty)
    }
      .toSet
  }

  private def handleOracleInteractionUntilFinished(entry: EntryPoint, projectTypes: Map[String, ClassFile])(implicit provider: MethodTACProvider): Unit = {
    var statusResponse = oracleApiClient.pullStatus()

    while(statusResponse.isSuccess && !statusResponse.get.hasFailed && statusResponse.get.isResolving) {

      val status = statusResponse.get

      if(status.requests.nonEmpty){
        log.info(s"Oracle has requested ${status.requests.size} method definitions from us.")

        status.requests.foreach{ request =>
          handleMethodRequest(request, projectTypes) match {
            case Success(response) =>
              log.info(s"Successfully generated response for request #${request.requestId} (method: ${request.mName} : ${request.mDescriptor})")
              oracleApiClient.pushResponse(response) match {
                case Success(_) =>
                  log.info(s"Successfully sent response #${request.requestId} to server.")
                case Failure(ex) =>
                  log.error(s"Failed to send response #${request.requestId} to server.", ex)
              }
            case Failure(ex) =>
              log.error(s"Failed to generate response for request #${request.requestId}", ex)
              oracleApiClient.pushResponse(LookupResponse(request.requestId, Set.empty, Set.empty, hasFatalErrors = true))
          }
        }
      } else {
        // Wait until action is needed
        Thread.sleep(1000)
      }
      statusResponse = oracleApiClient.pullStatus()
    }

    if(statusResponse.isFailure){
      log.error(s"Failed to pull oracle status from server", statusResponse.failed.get)
    } else if(statusResponse.get.hasFailed){
      log.error(s"Oracle encountered a fatal error: ${statusResponse.get.fatalError.getOrElse("NO INFO")}")
    } else {
      log.info(s"Done resolving entry point ${entry.callingContext.descriptor.toJava(entry.callingContext.name)}")
    }
  }

  private def handleMethodRequest(request: LookupRequestRepresentation, projectTypes: Map[String, ClassFile])(implicit provider: MethodTACProvider): Try[LookupResponse] = Try {

    val targetsFound: mutable.Set[ApplicationMethodWithSummaryRepr] = mutable.Set.empty
    val typesWithNoDef: mutable.Set[String] = mutable.Set.empty

    request.targetTypes.foreach { targetFqn =>
      projectTypes.get(targetFqn) match {
        case Some(targetClassFile) =>
          targetClassFile
            .methods
            .find(method => method.name == request.mName && method.descriptor.toJVMDescriptor == request.mDescriptor) match {
            case Some(method) =>

              val ifdsSummary = taintFlowSummaryBuilder.analyzeMethod(method).toResultRepresentation(true)
              targetsFound.add(ApplicationMethodWithSummaryRepr(oracleApiClient.opalToApiModel(method), ifdsSummary))
            case None =>
              typesWithNoDef.add(targetFqn)
          }
        case None =>
          log.error(s"Oracle requested information on a type that we do not know: $targetFqn")
      }
    }

    LookupResponse(request.requestId, targetsFound.toSet, typesWithNoDef.toSet, hasFatalErrors = false)
  }

  private case class EntryPoint(callingContext: Method, ccPC: Int, typesInitialized: Set[String])
}
