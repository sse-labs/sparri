package org.anon.spareuse.client.analyses

import org.anon.spareuse.client.http.SparriOracleApiClient
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.{LookupRequestRepresentation, LookupResponseRepresentation}
import org.anon.spareuse.execution.analyses.impl.ifds.IFDSTaintFlowSummaryBuilderImpl
import org.anon.spareuse.webapi.model.oracle.{ApplicationMethodRepr, LookupResponse, MethodIdentifierRepr, StartResolutionRequest, TypeNodeRepr}
import org.opalj.br.{ClassFile, Method}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.NEW
import org.opalj.tac.cg.CFA_1_1_CallGraphKey

import java.io.File
import java.net.URL
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

//TODO: Build a return type
class IFDSTaintFlowAnalysis(classesDirectory: File, pomFile: File) extends ClientAnalysis[Int](classesDirectory, pomFile) {

  private val remoteAnalysisName: String = IFDSTaintFlowSummaryBuilderImpl.analysisName
  private val remoteAnalysisVersion: String = "0.0.1"

  private val oracleApiClient: SparriOracleApiClient = new SparriOracleApiClient

  override def close(): Unit = {
    super.close()
    oracleApiClient.close()
  }

  override protected[analyses] def requirements: Seq[AnalysisRequirement] =
    Seq(AnalysisRequirement("com.google.code.gson:gson!com.google.code.gson:gson:2.11.0", remoteAnalysisName, remoteAnalysisVersion),
      AnalysisRequirement("com.google.errorprone:error_prone_annotations!com.google.errorprone:error_prone_annotations:2.27.0", remoteAnalysisName, remoteAnalysisVersion)) //TODO: Use real code once dependency extraction works
    /*getAllDependencies
      .map(dep => AnalysisRequirement(dep.identifier.toString, remoteAnalysisName, remoteAnalysisVersion))
      .toSeq*/

  override def execute(): Try[Int] = Try {
    val p = getOpalProject(loadJre = false)

    val fakeDependencies = Set("com.google.code.gson:gson:2.11.0", "com.google.errorprone:error_prone_annotations:2.27.0")

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

    Try(oracleApiClient.startOracleSession(fakeDependencies, projectTypeNodes, allTypesInitialized, 0, Some("17"))) match {
      case Success(_) =>
        log.info(s"Successfully started resolution session with server, session-id = ${oracleApiClient.getToken.getOrElse("<NONE>")}")

        // Give oracle time to fully initialize
        while(!oracleApiClient.isReadyForInteraction){
          Thread.sleep(500)
        }

        log.info(s"Oracle ready for interaction.")

        // Add all library entry points to a work stack
        val entryPointsToProcess = mutable.Stack.from(getLibraryEntryPoints(p))

        // Handle one entry point after the other
        while(entryPointsToProcess.nonEmpty){
          val currentEntry = entryPointsToProcess.pop()
          Try(oracleApiClient.startResolutionAt(currentEntry.callingContext, currentEntry.ccPC, currentEntry.typesInitialized)).flatten match {
            case Success(_) =>
              log.info(s"Successfully started resolution at entrypoint: ${currentEntry.callingContext.descriptor.toJava(currentEntry.callingContext.name)}")
              handleOracleInteractionUntilFinished(currentEntry, projectTypeMap)
            case Failure(ex) =>
              log.error(s"Failed to start resolution at entrypoint: ${currentEntry.callingContext.descriptor.toJava(currentEntry.callingContext.name)}", ex)
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
    //TODO: Implement detection of potential entry points
    // Currently this is just a demo that collects invocations from any main method
    val cg = project.get(CFA_1_1_CallGraphKey)

    Try(cg
      .reachableMethods()
      .filter(_.method.name == "main")
      .filter(_.method.hasSingleDefinedMethod)
      .flatMap{ ctx =>
        ctx
          .method
          .definedMethod
          .body
          .map( code => code
            .instructions
            .zipWithIndex
            .filter(t => t._1 != null && t._1.isInvocationInstruction && t._2 == 21)
            .map(_._2)
            .toSeq
          )
          .getOrElse(Seq.empty)
          .map( EntryPoint(ctx.method.definedMethod, _, Set.empty) )

      }
      .toSet) match {
      case Success(result) =>
        log.info(s"Found ${result.size} distinct library entry points.")
        result
      case Failure(ex) =>
        log.error("Failed to get library entry points", ex)
        Set.empty
    }
  }

  private def handleOracleInteractionUntilFinished(entry: EntryPoint, projectTypes: Map[String, ClassFile]): Unit = {
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
              // TODO: Send error (N)ACK
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

  private def handleMethodRequest(request: LookupRequestRepresentation, projectTypes: Map[String, ClassFile]): Try[LookupResponse] = Try {

    val targetsFound: mutable.Set[ApplicationMethodRepr] = mutable.Set.empty
    val typesWithNoDef: mutable.Set[String] = mutable.Set.empty

    request.targetTypes.foreach { targetFqn =>
      projectTypes.get(targetFqn) match {
        case Some(targetClassFile) =>
          targetClassFile
            .methods
            .find(method => method.name == request.mName && method.descriptor.toJVMDescriptor == request.mDescriptor) match {
            case Some(method) =>
              targetsFound.add(oracleApiClient.opalToApiModel(method))
            case None =>
              typesWithNoDef.add(targetFqn)
          }
        case None =>
          log.error(s"Oracle requested information on a type that we do not know: $targetFqn")
      }
    }

    LookupResponse(request.requestId, targetsFound.toSet, typesWithNoDef.toSet)
  }

  private case class EntryPoint(callingContext: Method, ccPC: Int, typesInitialized: Set[String])
}
