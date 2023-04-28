package de.tudo.sse.spareuse.execution

import akka.stream.scaladsl.Sink
import akka.{Done, NotUsed}
import akka.stream.scaladsl.Source
import de.tudo.sse.spareuse.core.formats.json.CustomObjectWriter
import de.tudo.sse.spareuse.core.maven.MavenIdentifier
import de.tudo.sse.spareuse.core.model.{AnalysisResultData, RunState}
import de.tudo.sse.spareuse.core.model.analysis.RunnerCommand
import de.tudo.sse.spareuse.core.model.entities.{MinerCommand, MinerCommandJsonSupport}
import de.tudo.sse.spareuse.core.storage.DataAccessor
import de.tudo.sse.spareuse.core.storage.postgresql.PostgresDataAccessor
import de.tudo.sse.spareuse.core.utils.http
import de.tudo.sse.spareuse.core.utils.rabbitmq.{MqMessageWriter, MqStreamIntegration}
import de.tudo.sse.spareuse.core.utils.streaming.AsyncStreamWorker
import de.tudo.sse.spareuse.execution.analyses.impl.{MvnConstantClassAnalysisImpl, MvnDependencyAnalysisImpl, MvnPartialCallgraphAnalysisImpl}
import de.tudo.sse.spareuse.execution.analyses.{AnalysisImplementation, AnalysisRegistry}
import de.tudo.sse.spareuse.execution.utils.{AnalysisRunNotPossibleException, ValidRunnerCommand, ValidStartRunCommand}
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

  private val dataAccessor: DataAccessor = new PostgresDataAccessor


  override def initialize(): Unit = {

    AnalysisRegistry.registerAnalysisImplementation(new MvnConstantClassAnalysisImpl)
    AnalysisRegistry.registerAnalysisImplementation(new MvnDependencyAnalysisImpl)
    AnalysisRegistry.registerAnalysisImplementation(new MvnPartialCallgraphAnalysisImpl)

    entityQueueWriter.initialize()

    dataAccessor.initialize()

    AnalysisRegistry.allAnalysisImplementations().foreach(d => dataAccessor.registerIfNotPresent(d.analysisData))
  }

  override def shutdown(): Unit = {
    dataAccessor.shutdown()
    entityQueueWriter.shutdown()
    super.shutdown()
  }

  override protected def buildSource(): Source[String, NotUsed] = createMqMessageSource(configuration, abortOnEmptyQueue = false)

  override protected def buildStreamPipeline(source: Source[String, NotUsed]): Future[Done] = {

    log.info(s"Listening for analysis commands on message queue '${configuration.mqQueueName}'.")

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
          if (!dataAccessor.hasAnalysisRun(analysisName, analysisVersion, command.runId))
            throw new AnalysisRunNotPossibleException("Designated run id not in DB: " + command.runId, command)

          // Assert that the required analysis implementation is available. This is the fastest requirement to check.
          if (AnalysisRegistry.analysisImplementationAvailable(analysisName, analysisVersion)) {
            val theAnalysisImpl = AnalysisRegistry.getAnalysisImplementation(analysisName, analysisVersion)

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

                if (!theAnalysisImpl.inputBatchProcessing)
                  throw new AnalysisRunNotPossibleException("Set of inputs contains an invalid entity name that can never be resolved: " + n, command)
              }

              isValidName
            }


            if (entityNamesToQueue.nonEmpty) {
              val deferredAnalysisCommand = if(!theAnalysisImpl.inputBatchProcessing || namesToProcess.diff(entityNamesNotIndexed).isEmpty) command// If no current run is executed, we do not need to generate a second run UID
              else {
                //If the current run is 'split into two', we need to generate a new run UID for the deferred analysis execution
                val emptyRunId = dataAccessor.storeEmptyAnalysisRun(analysisName, analysisVersion, command.configurationRaw).get
                command.updateRunId(emptyRunId)
              }

              // Will re-trigger this analysis, which works fine both in batch and non-batch mode
              val minerCommand = MinerCommand(entityNamesToQueue, Some(deferredAnalysisCommand))

              entityQueueWriter.appendToQueue(minerCommand.toJson.compactPrint, Some(2))

              if (!theAnalysisImpl.inputBatchProcessing) {
                log.warn("Analysis will be rescheduled once input mining is done.")
                return None
              }
            }


            namesToProcess = namesToProcess.diff(entityNamesNotIndexed)

            // Check that after all non-indexed names have been removed, there are in fact inputs left to process.
            if (namesToProcess.isEmpty) {

              if(entityNamesToQueue.isEmpty){
                log.error(s"No valid input entity names remain, and no deferred execution was possible for run ${command.runId}.")
                dataAccessor.setRunState(command.runId, RunState.Failed, Some(command.inputEntityNames))
              } else log.warn("No inputs left to process at this time")

              return None
            }

            // Download entity information for the analysis from the DB
            Try(namesToProcess.map(name => dataAccessor.getEntity(name, theAnalysisImpl.inputEntityKind, theAnalysisImpl.requiredInputResolutionLevel).get)) match {
              case Success(entities) =>
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
        Try(dataAccessor.setRunState(command.runId, RunState.Failed, Some(command.inputEntityNames)))
        None
    }

  }

  private def processRunnerCommand(command: ValidRunnerCommand): Future[Unit] = command match {
    case ValidStartRunCommand(cmd, inputEntities, analysisImpl) =>

      log.info(s"Starting analysis ${cmd.analysisName} with ${inputEntities.size} inputs. Requested by user ${cmd.userName}.")

      // Sets state of analysis run to "Running" and connects inputs to run
      Try(dataAccessor.setRunState(cmd.runId, RunState.Running, Some(inputEntities.map(_.uid)))) match {
        case Failure(ex) =>
          log.error("Failed to update analysis run record in db", ex)
        case _ =>
      }

      Future {
        analysisImpl.executeAnalysis(inputEntities.toSeq, cmd.configurationRaw) match {
          case Success(results) =>
            log.info(s"Analysis execution finished with ${results.size} results.")

            val serializer = new CustomObjectWriter(analysisImpl.analysisData.resultFormat)

            val resultUuidIt = dataAccessor.getFreshResultUuids(results.size).iterator

            val runResults = results.map{ res => AnalysisResultData(resultUuidIt.next(), isRevoked = false, res.content, res.affectedEntities) }

            val runLogs = analysisImpl.getLogs

            val dbRunId = cmd.runId

            dataAccessor.setRunResults(dbRunId, LocalDateTime.now(), runLogs.toArray, runResults)(serializer) match {
              case Success(_) =>
                log.info(s"Successfully stored ${results.size} results.")
              case Failure(ex) =>
                log.error("Failed to store analysis results", ex)
            }

          case Failure(ex) =>
            dataAccessor.setRunState(cmd.runId, RunState.Failed, None)
            log.error(s"Analysis execution failed.", ex)
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
    inputEntityNames.filterNot(name => dataAccessor.hasEntity(name, analysisImpl.inputEntityKind))
  }


  private def filterUnprocessedEntityNames(inputEntityNames: Set[String], analysisImpl: AnalysisImplementation)(implicit cmd: RunnerCommand): Set[String] = {
    dataAccessor.getAnalysisRuns(analysisImpl.name, analysisImpl.version) match {
      case Success(runData) =>
        if (analysisImpl.inputBatchProcessing) {
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
