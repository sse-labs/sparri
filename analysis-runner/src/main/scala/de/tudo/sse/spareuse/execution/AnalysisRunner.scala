package de.tudo.sse.spareuse.execution

import akka.stream.scaladsl.Sink
import akka.{Done, NotUsed}
import akka.stream.scaladsl.Source
import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, StartRunCommand}
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqStreamIntegration
import de.tudo.sse.spareuse.core.utils.streaming.AsyncStreamWorker
import de.tudo.sse.spareuse.execution.analyses.impl.{MvnConstantClassAnalysisImpl, MvnDependencyAnalysisImpl}
import de.tudo.sse.spareuse.execution.analyses.{AnalysisImplementation, AnalysisRegistry}
import de.tudo.sse.spareuse.execution.storage.impl.PostgresAdapter
import de.tudo.sse.spareuse.execution.storage.DataAccessor
import de.tudo.sse.spareuse.execution.utils.{AnalysisRunNotPossibleException, ValidRunnerCommand, ValidStartRunCommand}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class AnalysisRunner(private[execution] val configuration: AnalysisRunnerConfig)
  extends MqStreamIntegration
  with AsyncStreamWorker[String] {

  override val workerName: String = "analysis-runner"

  private def dataAccessor: DataAccessor = new PostgresAdapter()


  override def initialize(): Unit = {
    //TODO: Move Somewhere else
    AnalysisRegistry.registerAnalysisImplementation(new MvnConstantClassAnalysisImpl)
    AnalysisRegistry.registerAnalysisImplementation(new MvnDependencyAnalysisImpl)

    dataAccessor.initialize()
  }

  override def shutdown(): Unit = {
    dataAccessor.shutdown()
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
    RunnerCommand.fromJson(msg) match {
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

      command match {
        case startCmd: StartRunCommand if isSyntaxValid(startCmd) =>
          implicit val theCommand: StartRunCommand = startCmd

          log.debug(s"Validating command: User ${startCmd.userName} requests to start analysis ${startCmd.analysisName}.")

          val parts = startCmd.analysisName.split(":")
          val analysisName = parts(0)
          val analysisVersion = parts(1)

          // Assert that the required analysis implementation is available. This is the fastest requirement to check.
          if (AnalysisRegistry.analysisImplementationAvailable(analysisName, analysisVersion)) {
            val theAnalysisImpl = AnalysisRegistry.getAnalysisImplementation(analysisName, analysisVersion)

            // Check that analysis is registered in DB. Should always be the case
            ensureAnalysisIsRegistered(analysisName, analysisVersion)

            // Filter input entity names for ones that are actually present in DB
            val validEntityNames = filterValidEntityNames(startCmd.inputEntityNames, theAnalysisImpl)

            // Check whether parts of the results have already been computed, remove those inputs (if possible)
            val nonProcessedValidNames = filterUnprocessedEntityNames(validEntityNames, theAnalysisImpl)

            // Check that after all non-indexed names have been removed, there are in fact inputs left to process.
            if (nonProcessedValidNames.isEmpty) {
              throw new AnalysisRunNotPossibleException("No valid entity names that have not yet been processed left in input", startCmd)
            }

            // Download entity information for the analysis from the DB
            Try(nonProcessedValidNames.map(name => dataAccessor.getEntity(name, theAnalysisImpl.inputEntityKind, theAnalysisImpl.requiredInputResolutionLevel).get)) match {
              case Success(entities) =>
                // Finally check that the analysis can in fact be executed with those parameters
                if (theAnalysisImpl.executionPossible(entities.toSeq, startCmd.configurationRaw)) {
                  log.debug(s"Command successfully validated, analysis $analysisName:$analysisVersion will be started shortly.")
                  ValidStartRunCommand(startCmd, entities, theAnalysisImpl)
                } else throw AnalysisRunNotPossibleException("Given configuration not executable")
              case Failure(ex) =>
                throw AnalysisRunNotPossibleException("Failed to download entities from database", ex)
            }


          } else { throw AnalysisRunNotPossibleException("No matching analysis implementation registered.") }

        case sth@_ => throw new IllegalArgumentException("Unsupported runner command type: " + sth.getClass)
      }

    } match {
      case Success(validRunnerCommand) =>
        Some(validRunnerCommand)
      case Failure(ex) =>
        log.error("Command validation failed, no analysis will be executed.", ex)
        None
    }

  }

  private def processRunnerCommand(command: ValidRunnerCommand): Future[Unit] = command match {
    case ValidStartRunCommand(cmd, inputEntities, analysisImpl) =>

      log.info(s"Starting analysis ${cmd.analysisName} with ${inputEntities.size} inputs. Requested by user ${cmd.userName}.")

      Future {
        analysisImpl.executeAnalysis(inputEntities.toSeq, cmd.configurationRaw) match {
          case Success(results) =>
            log.info(s"Analysis execution finished with ${results.size} results.")
          //TODO: Store results
          case Failure(ex) =>
            log.error(s"Analysis execution failed.", ex)
          //TODO: Error Handling
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

  private def isSyntaxValid(cmd: StartRunCommand): Boolean = {
    val nameParts = cmd.analysisName.split(":")

    if(nameParts.length != 2 || nameParts(0).isBlank || nameParts(1).isBlank) false
    else if(cmd.userName.isBlank) false
    else if(cmd.inputEntityNames.isEmpty || cmd.inputEntityNames.forall(_.isBlank)) false
    else true

  }


  private def filterValidEntityNames(inputEntityNames: Set[String],
                                  analysisImpl: AnalysisImplementation)(implicit cmd: StartRunCommand): Set[String] = {

    val invalidEntityNames = inputEntityNames.filterNot(name => dataAccessor.hasEntity(name, analysisImpl.inputEntityKind))
    invalidEntityNames.foreach(n => log.warn(s"Input name $n of expected kind ${analysisImpl.inputEntityKind} not found."))

    // TODO: Trigger miner (with priority messages) for invalid entity names

    if (invalidEntityNames.nonEmpty && analysisImpl.inputBatchProcessing) {
      // This means individual entity names can be removed from the input list
      log.warn(s" ${invalidEntityNames.size} invalid input names have been removed for processing.")
      inputEntityNames.diff(invalidEntityNames)
    } else if (invalidEntityNames.nonEmpty) {
      // This means no entities can be removed without corrupting the result, so execution may not proceed
      log.error(s"Not all inputs could be found in the DB, analysis cannot be executed.")
      throw AnalysisRunNotPossibleException("Not all required inputs found in database")
    } else {
      // This means all entities are present, so we can proceed either way
      inputEntityNames
    }
  }

  private def filterUnprocessedEntityNames(inputEntityNames: Set[String], analysisImpl: AnalysisImplementation)(implicit cmd: StartRunCommand): Set[String] = {
    dataAccessor.getAnalysisRuns(analysisImpl.name, analysisImpl.version) match {
      case Success(runData) =>
        if (analysisImpl.inputBatchProcessing) {
          val allInputsProcessed = runData.flatMap(_.inputs.map(_.uid))
          inputEntityNames.diff(allInputsProcessed)
        } else {
          val allInputSets = runData.map(_.inputs.map(_.uid))
          if (allInputSets.contains(inputEntityNames)) Set.empty
          else inputEntityNames
        }
      case Failure(ex) =>
        throw AnalysisRunNotPossibleException("Failed to check database for analysis runs",  ex)
    }
  }

  private def ensureAnalysisIsRegistered(analysisName: String, analysisVersion: String)(implicit cmd: StartRunCommand): Unit = {
    dataAccessor.getAnalysisData(analysisName, analysisVersion) match {
      case Success(analysisData) if analysisData.isRevoked =>
        log.warn(s"Analysis $analysisName:$analysisVersion has been revoked, results may not be valid.")
      case Failure(ex) =>
        log.error("Analysis not registered in DB, cannot report any results", ex)
        throw AnalysisRunNotPossibleException("Analysis not registered in database,", ex)
      case _ =>
    }
  }
}
