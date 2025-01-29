package org.anon.spareuse.client.analyses.ifds

import org.anon.spareuse.client.analyses.LocalMavenClientAnalysis
import org.anon.spareuse.client.http.SparriOracleApiClient
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.LookupRequestRepresentation
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphResolutionMode
import org.anon.spareuse.execution.analyses.impl.ifds.reachability.IFDSMethodRunner
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSFact, IFDSTaintFlowSummaryBuilderImpl, IFDSZeroFact, MethodTACProvider}
import org.anon.spareuse.webapi.model.oracle.{ApplicationMethodWithSummaryRepr, LookupResponse, TypeNodeRepr}
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.cg.ApplicationEntryPointsFinder
import org.opalj.br.instructions.NEW
import org.opalj.br.{ClassFile, Method}
import org.opalj.tac.ComputeTACAIKey
import org.opalj.tac.cg.{CallGraph, RTACallGraphKey}

import java.net.URL
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.util.{Failure, Success, Try}

class IFDSTaintFlowAnalysis(mavenProjectDir: Path) extends LocalMavenClientAnalysis[Int](mavenProjectDir) {

  private val remoteAnalysisName: String = IFDSTaintFlowSummaryBuilderImpl.analysisName
  private val remoteAnalysisVersion: String = "0.0.5"

  private val oracleApiClient: SparriOracleApiClient = new SparriOracleApiClient

  // (Ab)use the existing taint flow summary builder, which is normally used in the context of an AnalysisRunner instance
  private val taintFlowSummaryBuilder: IFDSTaintFlowSummaryBuilderImpl = new IFDSTaintFlowSummaryBuilderImpl(None)

  private var noOfLookups: Int = 0
  private var noOfTargetsSent: Int = 0
  private val methodSummaryCache: mutable.Map[Method, ApplicationMethodWithSummaryRepr] = mutable.HashMap.empty

  private def noOfMethodsAnalyzed: Int = methodSummaryCache.size

  override def close(): Unit = {
    super.close()
    oracleApiClient.close()
  }

  override protected[analyses] def requirements: Seq[AnalysisRequirement] =
    mavenDependenciesTry
      .get // Note that any exceptions thrown here will be caught by the calling (final) method ClientAnalysis.checkRequirements()
      .map(dep => AnalysisRequirement(dep.identifier.toGA + "!" + dep.identifier.version, remoteAnalysisName, remoteAnalysisVersion))
      .toSeq

  override def execute(arguments: Array[String]): Try[Int] = Try {
    val p = getOpalProject(loadJre = false)

    val cg = p.get(RTACallGraphKey)

    val dependencies = mavenDependenciesTry.get.map(_.identifier.toString)

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

    val libraryEntryPoints = mutable.HashSet.empty[MethodIdent]

    Try(oracleApiClient.startOracleSession(dependencies, projectTypeNodes, allTypesInitialized, OracleCallGraphResolutionMode.NaiveRTA.id, Some("17"))) match {
      case Success(_) =>
        log.info(s"Successfully started resolution session with server, session-id = ${oracleApiClient.getToken.getOrElse("<NONE>")}")

        // Give oracle time to fully initialize
        while(!oracleApiClient.isReadyForInteraction){
          Thread.sleep(100)
        }

        val startTime = System.currentTimeMillis()

        log.info(s"Oracle ready for interaction.")

        // Add all library entry points to a work stack
        val entryPointsToProcess = mutable.Stack.from(getLibraryEntryPoints(p, cg))

        val entryCnt = entryPointsToProcess.size
        var currEntry = 0

        log.info(s"Found a total of $entryCnt library entry points.")

        // Handle one entry point after the other
        while(entryPointsToProcess.nonEmpty){
          val currentEntry = entryPointsToProcess.pop()
          Try {
            // IMPROVE: Use summary cache here, entrypoint might have been reached before!
            val summary = taintFlowSummaryBuilder.analyzeMethod(currentEntry.callingContext).toResultRepresentation(true)
            log.info(s"Starting resolution for entrypoint ${currentEntry.callingContext.descriptor.toJava(currentEntry.callingContext.name)}")
            oracleApiClient.startResolutionAt(currentEntry.callingContext, summary, currentEntry.ccPC, currentEntry.typesInitialized)
          } match {
            case Success(_) =>
              log.info(s"Successfully started resolution for entrypoint $currEntry / $entryCnt")
              handleOracleInteractionUntilFinished(currentEntry, projectTypeMap)
              libraryEntryPoints.add(currentEntry.methodCalled)
            case Failure(ex) =>
              log.error(s"Failed to start resolution at entrypoint: ${currentEntry.callingContext.descriptor.toJava(currentEntry.callingContext.name)} , PC=${currentEntry.ccPC}", ex)
          }
          currEntry += 1
        }

        var durationMainLoop = (System.currentTimeMillis() - startTime) / 1000
        oracleApiClient.finalizeSession() match {
          case Success(_) =>
            durationMainLoop = (System.currentTimeMillis() - startTime) / 1000
            log.info(s"Successfully finalized resolution session. Stats:")
            log.info(s"\t - Number of lookup request by oracle: $noOfLookups")
            log.info(s"\t - Number of methods summarized: $noOfMethodsAnalyzed")
            log.info(s"\t - Number of method summaries sent to oracle: $noOfTargetsSent")
            log.info(s"\t - Duration of main resolution loop: $durationMainLoop sec")
          case Failure(ex) =>
            log.error(s"Failure during session finalization", ex)
        }

        val queryStart = System.currentTimeMillis()
        val allQueries = getFactsAtLibraryEntryPoints(p, cg, libraryEntryPoints.toSet)

        log.info(s"Starting to query ${allQueries.size} library entry points")

        allQueries.foreach{ libEntryQuery =>
          oracleApiClient.doQuery(libEntryQuery.ident, libEntryQuery.factsAtEntry) match {
            case Success(facts) =>
              log.info(s"Invoking ${libEntryQuery.ident} resulted in facts: ${facts.map(_.displayName).mkString(",")}")
            case Failure(ex) =>
              if(!ex.getMessage.contains("unknown method"))
                log.error(s"Failed to query library entry point ${libEntryQuery.ident}", ex)
              else
                log.error(s"Server did not know summary for ${libEntryQuery.ident}")
          }

        }

        val queryDuration = (System.currentTimeMillis() - queryStart) / 1000

        if(arguments.nonEmpty){
          val outPath = Paths.get(arguments(0))
          if(!outPath.toFile.exists() || !outPath.toFile.isDirectory){
            log.error(s"Cannot write statistics to file, the given output directory is not valid: ${arguments(0)}")
          } else {
            val statistics = Map(
              "lookup-requests-received" -> noOfLookups,
              "methods-summarized-on-demand" -> noOfMethodsAnalyzed,
              "methods-summaries-sent" -> noOfTargetsSent,
              "duration-main-loop-seconds" -> durationMainLoop,
              "duration-query-loop" -> queryDuration
            )
            val outFile = outPath.resolve("modular-stats.log")
            Files.write(outFile, statistics.map{ case (k,v) => s"$k: $v"}.toSeq.asJava)
          }
        }



        oracleApiClient.closeSession() match {
          case Success(_) =>
            log.info(s"Successfully closed oracle session")
          case Failure(ex) =>
            log.error(s"Failed to close session at oracle", ex)
        }

        0
      case Failure(ex) =>
        log.error(s"Failed to start oracle session", ex)
        throw ex
    }

  }

  private def getLibraryEntryPoints(project: Project[URL], cg: CallGraph): Set[EntryPoint] = {
    cg
      .reachableMethods()
      .flatMap(ctx => cg.calleesOf(ctx.method).flatMap(t => t._2.map(t2 => (ctx, t._1, t2))))
      .filter{
        case (caller, _, callee) =>
          !project.isProjectType(callee.method.declaringClassType) && project.isProjectType(caller.method.declaringClassType) && !callee.method.declaringClassType.fqn.startsWith("java")
      }.map{
      case (callerCtx, pc, callee) =>
        EntryPoint(callerCtx.method.definedMethod, pc, Set.empty, MethodIdent(callee.method.declaringClassType.fqn, callee.method.name, callee.method.descriptor.toJVMDescriptor))
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
        Thread.sleep(20)
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

    noOfLookups += 1

    val targetsFound: mutable.Set[ApplicationMethodWithSummaryRepr] = mutable.Set.empty
    val typesWithNoDef: mutable.Set[String] = mutable.Set.empty

    request.targetTypes.foreach { targetFqn =>
      projectTypes.get(targetFqn) match {
        case Some(targetClassFile) =>
          targetClassFile
            .methods
            .find(method => method.name == request.mName && method.descriptor.toJVMDescriptor == request.mDescriptor) match {
            case Some(method) if methodSummaryCache.contains(method) =>
              targetsFound.add(methodSummaryCache(method))
            case Some(method) =>
              val ifdsSummary = taintFlowSummaryBuilder.analyzeMethod(method).toResultRepresentation(true)
              val methodSummary = ApplicationMethodWithSummaryRepr(oracleApiClient.opalToApiModel(method), ifdsSummary)
              methodSummaryCache.put(method, methodSummary)
              targetsFound.add(methodSummary)
            case None =>
              typesWithNoDef.add(targetFqn)
          }
        case None =>
          log.error(s"Oracle requested information on a type that we do not know: $targetFqn")
      }
    }

    noOfTargetsSent += targetsFound.size

    LookupResponse(request.requestId, targetsFound.toSet, typesWithNoDef.toSet, hasFatalErrors = false)
  }

  private def getFactsAtLibraryEntryPoints(opalProject: Project[URL],
                                           cg: CallGraph,
                                           libraryEntryPoints: Set[MethodIdent])(implicit provider: MethodTACProvider): Set[IFDSQuery] = {
    val programEntryPoints = ApplicationEntryPointsFinder
      .collectEntryPoints(opalProject)
      .map(m => MethodIdent(m.classFile.fqn, m.name, m.descriptor.toJVMDescriptor))

    log.info(s"Found ${programEntryPoints.size} program entry points.")

    log.info(s"Building remaining summaries...")
    val summaryDict = opalProject
      .allProjectClassFiles
      .flatMap(_.methods)
      .flatMap { method =>
        val ident = MethodIdent(method.classFile.fqn, method.name, method.descriptor.toJVMDescriptor)
        Some(ident, taintFlowSummaryBuilder.analyzeMethod(method))
      }
      .toMap

    log.info("Detecting facts at library entry points ...")
    val modularRunnerEnv = new ModularIFDSRunnerEnvironment(cg, summaryDict, libraryEntryPoints)
    val modularIFDSRunner = new IFDSMethodRunner(modularRunnerEnv)
    programEntryPoints
      .foreach{ programEntry =>
        modularIFDSRunner.resolveFrom(programEntry, Set(IFDSZeroFact))
      }
    log.info(s"Done detecting facts at library entry points.")

    modularRunnerEnv.getAllQueries
  }

  private case class EntryPoint(callingContext: Method, ccPC: Int, typesInitialized: Set[String], methodCalled: MethodIdent)
}
