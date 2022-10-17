package de.tudo.sse.spareuse.execution

import akka.stream.scaladsl.Sink
import akka.{Done, NotUsed}
import akka.stream.scaladsl.Source
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqStreamIntegration
import de.tudo.sse.spareuse.core.utils.streaming.AsyncStreamWorker
import de.tudo.sse.spareuse.execution.commands.RunnerCommand

import scala.concurrent.Future
import scala.util.{Failure, Success}

class AnalysisRunner(private[execution] val configuration: AnalysisRunnerConfig)
  extends MqStreamIntegration
  with AsyncStreamWorker[String] {

  override val workerName: String = "analysis-runner"

  override def initialize(): Unit = ???

  override protected def buildSource(): Source[String, NotUsed] = createMqMessageSource(configuration)

  override protected def buildStreamPipeline(source: Source[String, NotUsed]): Future[Done] = {
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

  private def validatePrerequisites(command: RunnerCommand): Option[RunnerCommand] = ???

  private def executeAnalysis(command: RunnerCommand): Future[Unit] = ???

  private def buildExecutionSink(): Sink[RunnerCommand, Future[Done]] = {
    Sink.foreachAsync(configuration.executionPoolSize) { cmd =>

      executeAnalysis(cmd).andThen {
        case Success(_) =>
          log.info(s"Execution for command finished successfully: $cmd")
        case Failure(ex) =>
          log.error(s"Failed to execute command: $cmd", ex)
      }(streamMaterializer.executionContext)

    }
  }
}
