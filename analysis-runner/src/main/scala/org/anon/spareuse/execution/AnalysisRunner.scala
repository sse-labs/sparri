package org.anon.spareuse.execution

import akka.stream.scaladsl.Sink
import akka.{Done, NotUsed}
import akka.stream.scaladsl.Source
import org.anon.spareuse.core.model.{AnalysisResultData, RunState}
import org.anon.spareuse.core.model.entities.{MinerCommand, MinerCommandJsonSupport}
import org.anon.spareuse.core.utils.rabbitmq.{MqMessageWriter, MqStreamIntegration}
import org.anon.spareuse.execution.utils.{AnalysisRunNotPossibleException, ValidRunnerCommand, ValidStartRunCommand}
import org.anon.spareuse.core.formats.json.CustomObjectWriter
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.analysis.{AnalysisCommand, IncrementalAnalysisCommand, RunnerCommand}
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.storage.postgresql.PostgresDataAccessor
import org.anon.spareuse.core.utils.{http, wcTime}
import org.anon.spareuse.core.utils.streaming.AsyncStreamWorker
import org.anon.spareuse.execution.analyses.impl.cg.JreModelLoader
import org.anon.spareuse.execution.analyses.impl.ifds.{DefaultIFDSSummaryBuilder, IFDSTaintFlowSummaryBuilderImpl}
import org.anon.spareuse.execution.analyses.impl.{MvnConstantClassAnalysisImpl, MvnConstantMethodsAnalysisImpl, MvnDependencyAnalysisImpl, MvnPartialCallgraphAnalysisImpl}
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisRegistry, ExistingResult, FreshResult}
import spray.json.{enrichAny, enrichString}

import java.time.LocalDateTime
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class AnalysisRunner(private[execution] val configuration: AnalysisRunnerConfig)
  extends MqStreamIntegration
  with AsyncStreamWorker[String]
  with MinerCommandJsonSupport {

  private val entityQueueWriter = new MqMessageWriter(configuration.toWriterConfig)

  override val workerName: String = "analysis-runner"

  private val dataAccessor: DataAccessor = new PostgresDataAccessor()(streamMaterializer.executionContext)


  override def initialize(): Unit = {
    // Load information about JRE representations. Representations themselves will be index lazily
    JreModelLoader.indexJreData(configuration.jreDataDir)

    AnalysisRegistry.registerRegularAnalysis(MvnConstantClassAnalysisImpl, () => new MvnConstantClassAnalysisImpl)
    AnalysisRegistry.registerRegularAnalysis(MvnDependencyAnalysisImpl, () => new MvnDependencyAnalysisImpl)
    AnalysisRegistry.registerRegularAnalysis(MvnPartialCallgraphAnalysisImpl, () => new MvnPartialCallgraphAnalysisImpl)
    AnalysisRegistry.registerRegularAnalysis(MvnConstantMethodsAnalysisImpl, () => new MvnConstantMethodsAnalysisImpl)
    AnalysisRegistry.registerIncrementalAnalysis(IFDSTaintFlowSummaryBuilderImpl.descriptor, opt => new IFDSTaintFlowSummaryBuilderImpl(opt))

    entityQueueWriter.initialize()

    dataAccessor.initialize()

    AnalysisRegistry.allAnalysisDescriptors().foreach(d => dataAccessor.registerIfNotPresent(d.analysisData))
  }

  override def shutdown(): Unit = {
    dataAccessor.shutdown()
    entityQueueWriter.shutdown()
    super.shutdown()
  }

  override protected def buildSource(): Source[String, NotUsed] = createMqMessageSource(configuration.toReaderConfig, abortOnEmptyQueue = false)

  override protected def buildStreamPipeline(source: Source[String, NotUsed]): Future[Done] = {

    log.info(s"Listening for analysis commands on message queue '${configuration.toReaderConfig.mqQueueName}'.")

    source
      .map(parseCommand)
      .filter(_.isDefined)
      .map(cmdOpt => validatePrerequisites(cmdOpt.get))
      .filter(_.isDefined)
      .map(_.get)
      .runWith(buildExecutionSink())(streamMaterializer)
  }

  private def parseCommand(msg: String): Option[RunnerCommand] = {
    Try(msg.parseJson.convertTo[RunnerCommand]) match {
      case Success(cmd) => Some(cmd)
      case Failure(ex) =>
        log.error(s"Message from queue could not be parsed: $msg", ex)
        None
    }
  }

  /**
   * This methods validates the given command and returns a validated runner command. Currently supports only StartRunCommands,
   * the validation of which involves:
   *
   * 1.) Ensuring that an implementation for the requested analysis is registered at this runner
   * 2.) Ensuring that the analysis metadata is available in the DB
   * 3.) Filter for input entity names that are actually in the DB
   * 4.) Filter inputs for existing results of the requested analysis if possible
   * 5.) Download input object structure from DB
   * 6.) Validate that the execution of the analysis implementation is possible with the given inputs and configuration
   *
   * @param command The parsed command object to validate
   * @return None if the command was not valid, otherwise an Option containing the ValidRunnerCommand
   */
  private def validatePrerequisites(command: RunnerCommand): Option[ValidRunnerCommand] = {
    Try {

        if (isSyntaxValid(command)) {
          implicit val theCommand: RunnerCommand = command

          log.info(s"Validating command: User ${command.userName} requests to start analysis ${command.analysisName}.")

          val parts = command.analysisName.split(":")
          val analysisName = parts(0)
          val analysisVersion = parts(1)

          // Assert that empty run entry has been created beforehand
          if (!dataAccessor.hasAnalysisRun(analysisName, analysisVersion, command.associatedRunId))
            throw new AnalysisRunNotPossibleException("Designated run id not in DB: " + command.associatedRunId, command)

          // Check whether an implementation is registered and return a new implementation instance
          val implementationOpt = command match {
            case IncrementalAnalysisCommand(_, _, _, _, _, baselineRunId) =>
              if (AnalysisRegistry.hasIncrementalAnalysisImplementation(analysisName, analysisVersion)) {

                val baselineRun = if(baselineRunId.isBlank) None
                else {
                  if(dataAccessor.hasAnalysisRun(analysisName, analysisVersion, baselineRunId)){

                    // Make sure the run that we pass to the analysis as a baseline has fully resolved entity hierarchies
                    val theRun = dataAccessor
                      .getAnalysisRun(analysisName, analysisVersion, baselineRunId, includeResults = true)
                      .get
                      .withResolvedGenerics( data => dataAccessor.awaitGetEntity(data.uid, None).get , forceResolve = true)

                    Some(theRun)
                  } else
                    throw new AnalysisRunNotPossibleException(s"Baseline run ID invalid: " + baselineRunId, command)
                }

                log.info(s"Done resolving baseline for run $baselineRunId.")
                Some(AnalysisRegistry.getIncrementalAnalysisImplementation(analysisName, analysisVersion, baselineRun))
              } else None

            case _: AnalysisCommand =>
              if (AnalysisRegistry.hasRegularAnalysisImplementation(analysisName, analysisVersion))
                Some(AnalysisRegistry.getRegularAnalysisImplementation(analysisName, analysisVersion))
              else None

            case _ =>
              None

          }

          // Assert that the required analysis implementation is available. This is the fastest requirement to check.
          if (implementationOpt.isDefined) {
            val theAnalysisImpl = implementationOpt.get

            // Check that analysis is registered in DB. Should always be the case
            ensureAnalysisIsRegistered(analysisName, analysisVersion)

            // Filter out all input entities for which results already exist (if analysis is batch processing)
            var namesToProcess = filterUnprocessedEntityNames(command.inputEntityNames, theAnalysisImpl)

            // If some inputs are not indexed yet:
            //  - For Batch analyses we can execute the analysis now with all indexed inputs, and queue non-indexed inputs for mining.
            //    After the mining is done, a callback will re-trigger the analysis for the newly indexed inputs. If there are inputs
            //    that are not indexed and not valid (i.e. mining will never succeed), we print a warning and not queue them for mining.
            //  - For Non-Batch analyses we have to queue all missing inputs for mining and wait for the callback to re-trigger the
            //    analysis. If there are inputs that are not indexed and not valid (i.e. mining will never succeed), we can throw a
            //    terminal error, since the analysis can never be executed with this input configuration.
            val entityNamesNotIndexed = getEntityNamesNotInDb(namesToProcess, theAnalysisImpl)

            val entityNamesToQueue = entityNamesNotIndexed.filter { n =>
              val isValidName = isValidEntityName(n)

              if (!isValidName) {
                log.warn("Input is not a valid entity name: " + n)

                if (!theAnalysisImpl.descriptor.inputBatchProcessing)
                  throw new AnalysisRunNotPossibleException("Set of inputs contains an invalid entity name that can never be resolved: " + n, command)
              }

              isValidName
            }


            if (entityNamesToQueue.nonEmpty) {
              val deferredAnalysisCommand = if(!theAnalysisImpl.descriptor.inputBatchProcessing || namesToProcess.diff(entityNamesNotIndexed).isEmpty) command// If no current run is executed, we do not need to generate a second run UID
              else {
                //If the current run is 'split into two', we need to generate a new run UID for the deferred analysis execution
                val emptyRunId = dataAccessor.storeEmptyAnalysisRun(analysisName, analysisVersion, command.configurationRaw).get
                command.updateRunId(emptyRunId)
              }

              // Will re-trigger this analysis, which works fine both in batch and non-batch mode
              val minerCommand = MinerCommand(entityNamesToQueue, Some(deferredAnalysisCommand))

              entityQueueWriter.appendToQueue(minerCommand.toJson.compactPrint, Some(2))

              if (!theAnalysisImpl.descriptor.inputBatchProcessing) {
                log.warn("Analysis will be rescheduled once input mining is done.")
                return None
              }
            }


            namesToProcess = namesToProcess.diff(entityNamesNotIndexed)

            // Check that after all non-indexed names have been removed, there are in fact inputs left to process.
            if (namesToProcess.isEmpty) {

              if(entityNamesToQueue.isEmpty){
                log.error(s"No valid input entity names remain, and no deferred execution was possible for run ${command.associatedRunId}.")
                dataAccessor.setRunState(command.associatedRunId, RunState.Failed, Some(command.inputEntityNames))
              } else log.warn("No inputs left to process at this time")

              return None
            }

            log.info(s"Starting to download entity information from DB: ${namesToProcess.mkString(",")}")

            // Download entity information for the analysis from the DB
            Try(namesToProcess.map(name => dataAccessor.awaitGetEntity(name, theAnalysisImpl.descriptor.requiredInputResolutionLevel).get)) match {
              case Success(entities) =>
                log.info(s"Done downloading entity information for ${entities.size} entities")
                // Finally check that the analysis can in fact be executed with those parameters
                if (theAnalysisImpl.executionPossible(entities.toSeq, command.configurationRaw)) {
                  log.info(s"Command successfully validated, analysis $analysisName:$analysisVersion will be started shortly.")
                  ValidStartRunCommand(command, entities, theAnalysisImpl)
                } else throw AnalysisRunNotPossibleException("Given configuration not executable")
              case Failure(ex) =>
                throw AnalysisRunNotPossibleException("Failed to download entities from database", ex)
            }


          } else {
            throw AnalysisRunNotPossibleException("No matching analysis implementation registered.")
          }

        } else {
          throw AnalysisRunNotPossibleException("Command syntax invalid")(command)
        }
    } match {
      case Success(validRunnerCommand) =>
        Some(validRunnerCommand)
      case Failure(ex) =>
        log.error("Command validation failed, no analysis will be executed.", ex)
        Try(dataAccessor.setRunState(command.associatedRunId, RunState.Failed, Some(command.inputEntityNames)))
        None
    }

  }

  private def processRunnerCommand(command: ValidRunnerCommand): Future[Unit] = command match {
    case ValidStartRunCommand(cmd, inputEntities, analysisImpl) =>

      log.info(s"Starting analysis ${cmd.analysisName} with ${inputEntities.size} inputs. Requested by user ${cmd.userName}.")

      // Sets state of analysis run to "Running" and connects inputs to run
      Try(dataAccessor.setRunState(cmd.associatedRunId, RunState.Running, Some(inputEntities.map(_.uid)))) match {
        case Failure(ex) =>
          log.error("Failed to update analysis run record in db", ex)
        case _ =>
      }

      Future {
        val timedResult = wcTime { () => analysisImpl.executeAnalysis(inputEntities.toSeq, cmd.configurationRaw) }

        timedResult.result match {
          case Success(results) =>

            val numberOfFreshResults = results.count( _.isFresh )

            log.info(s"Analysis execution finished with ${results.size} results ($numberOfFreshResults fresh results) in ${timedResult.timeMs}ms.")

            val serializer = new CustomObjectWriter(analysisImpl.descriptor.analysisData.resultFormat)

            val resultUuidIt = dataAccessor.getFreshResultUuids(numberOfFreshResults).iterator

            val freshResults = results.collect{
              case FreshResult(content, affectedEntities) =>
                AnalysisResultData(resultUuidIt.next(), isRevoked = false, cmd.associatedRunId, content, affectedEntities)
            }

            val unchangedResultIds = results.collect{
              case ExistingResult(resultUid) => resultUid
            }

            val runLogs = analysisImpl.getLogs

            val dbRunId = cmd.associatedRunId

            dataAccessor.setRunResults(dbRunId, LocalDateTime.now(), timedResult.timeMs, runLogs.toArray, freshResults, unchangedResultIds)(serializer) match {
              case Success(_) =>
                log.info(s"Successfully stored ${results.size} results.")
              case Failure(ex) =>
                log.error("Failed to store analysis results", ex)
            }

          case Failure(ex) =>
            dataAccessor.setRunState(cmd.associatedRunId, RunState.Failed, None)
            log.error(s"Analysis execution failed in ${timedResult.timeMs}ms.", ex)
        }
      }(this.streamMaterializer.executionContext)

      case _ =>
        log.error(s"Execution of runner command not implemented: ${command.runnerCommand}")
        Future.failed(new NotImplementedError())
  }

  private def buildExecutionSink(): Sink[ValidRunnerCommand, Future[Done]] = {
    Sink.foreachAsync(configuration.executionPoolSize) { cmd =>

      processRunnerCommand(cmd).andThen {
        case Success(_) =>
          log.info(s"Execution for command finished successfully: ${cmd.runnerCommand}")
        case Failure(ex) =>
          log.error(s"Failed to execute command: $cmd", ex)
      }(streamMaterializer.executionContext)

    }
  }


  // --------------------------------------------------------------------------
  // |                       COMMAND VALIDATION UTILS                         |
  // --------------------------------------------------------------------------

  private def isSyntaxValid(cmd: RunnerCommand): Boolean = {
    val nameParts = cmd.analysisName.split(":")

    if(nameParts.length != 2 || nameParts(0).isBlank || nameParts(1).isBlank) false
    else if(cmd.userName.isBlank) false
    else if(cmd.inputEntityNames.isEmpty || cmd.inputEntityNames.forall(_.isBlank)) false
    else true

  }

  private def getEntityNamesNotInDb(inputEntityNames: Set[String], analysisImpl: AnalysisImplementation): Set[String] = {
    inputEntityNames.filterNot(name => dataAccessor.hasEntity(name, analysisImpl.descriptor.inputEntityKind))
  }


  private def filterUnprocessedEntityNames(inputEntityNames: Set[String], analysisImpl: AnalysisImplementation)(implicit cmd: RunnerCommand): Set[String] = {
    dataAccessor.getAnalysisRuns(analysisImpl.descriptor.name, analysisImpl.descriptor.version) match {
      case Success(runData) =>
        if (analysisImpl.descriptor.inputBatchProcessing) {
          val allInputsProcessed = runData
            .filter(_.state.equals(RunState.Finished)) // Only consider non-failed runs!
            .flatMap(_.inputs.map(_.uid))
          inputEntityNames.diff(allInputsProcessed)
        } else {
          val allInputSets = runData
            .filter(_.state.equals(RunState.Finished)) // Only consider non-failed runs!
            .map(_.inputs.map(_.uid))
          if (allInputSets.contains(inputEntityNames)) Set.empty
          else inputEntityNames
        }
      case Failure(ex) =>
        throw AnalysisRunNotPossibleException("Failed to check database for analysis runs",  ex)
    }
  }

  private def ensureAnalysisIsRegistered(analysisName: String, analysisVersion: String)(implicit cmd: RunnerCommand): Unit = {
    dataAccessor.getAnalysisData(analysisName, analysisVersion) match {
      case Success(analysisData) if analysisData.isRevoked =>
        log.warn(s"Analysis $analysisName:$analysisVersion has been revoked, results may not be valid.")
      case Failure(ex) =>
        log.error("Analysis not registered in DB, cannot report any results", ex)
        throw AnalysisRunNotPossibleException("Analysis not registered in database,", ex)
      case _ =>
    }
  }


  private def isValidEntityName(name: String): Boolean = {

    if(name.isBlank) return false

    val parts = name.split("!")
    if(parts.isEmpty){ false }
    else if (parts.length == 1){
      // Library only, so ensure we have G:A format
      val ga = parts(0).split(":")

      if(ga.length == 2 && !ga(0).isBlank && !ga(1).isBlank){
        val libUri = MavenIdentifier(MavenIdentifier.DefaultRepository, ga(0), ga(1), "").toLibLocation
        http.checkUriExists(libUri.toString)
      } else {
        false
      }

    } else {
      // Program, so ensure we have G:A!G:A:V
      val gav = parts(1).split(":")
      if(gav.length == 3 && gav.forall(s => !s.isBlank)){
        val artifactUri = MavenIdentifier(MavenIdentifier.DefaultRepository, gav(0), gav(1), gav(2)).toPomLocation
        http.checkUriExists(artifactUri.toString)
      } else {
        false
      }
    }


  }
}
