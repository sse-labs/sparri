package de.tudo.sse.spareuse.core.utils.streaming

import akka.actor.ActorSystem
import akka.stream.{KillSwitches, Materializer, SharedKillSwitch}
import akka.stream.scaladsl.{Keep, Source}
import akka.{Done, NotUsed}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait AsyncStreamWorker[E] {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  val workerName: String

  protected lazy val actorSystem: ActorSystem = ActorSystem(workerName)
  protected lazy val streamMaterializer: Materializer = Materializer(actorSystem)
  protected lazy val streamKillSwitch: SharedKillSwitch = KillSwitches.shared(s"$workerName-kill-switch")

  private var currentProcessingFuture: Option[Future[Done]] = None


  def initialize(): Unit

  def shutdown(): Unit = {
    streamMaterializer.shutdown()
    Await.ready(actorSystem.terminate(), 30.seconds)
  }

  def startWork(): Future[Done] = {
    Try(buildSource()) match {
      case Success(source) =>
        val stoppableSource = source.viaMat(streamKillSwitch.flow)(Keep.left)

        val pipelineFuture = buildStreamPipeline(stoppableSource)

        currentProcessingFuture = Some(pipelineFuture)

        pipelineFuture
      case Failure(ex) =>
        log.error("Failed to initialize source", ex)
        currentProcessingFuture = None
        Future.failed(ex)
    }


  }

  def stopWork(): Unit = {
    streamKillSwitch.shutdown()

    currentProcessingFuture match {
      case Some(future) => Await.ready(future, 10.second)
      case _ =>
    }

    log.info("Stream processing stopped.")
  }


  protected def buildSource(): Source[E, NotUsed]

  protected def buildStreamPipeline(source: Source[E, NotUsed]): Future[Done]

  private[streaming] def getExecutionContext: ExecutionContext = streamMaterializer.executionContext

}
