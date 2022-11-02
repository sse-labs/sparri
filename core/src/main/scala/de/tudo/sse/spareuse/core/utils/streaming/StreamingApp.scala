package de.tudo.sse.spareuse.core.utils.streaming

import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

trait StreamingApp[T] {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  private var keyPressFuture: Option[Future[Unit]] = None

  def main(args: Array[String]): Unit = {

    loadConfig() match {
      case Some(config) =>
        val worker = buildWorker(config)

        Try(worker.initialize()) match {
          case Success(_) =>
            val workFuture = worker.startWork()

            workFuture
              .onComplete(_ => {
                worker.shutdown()
                onComplete()
              })(worker.getExecutionContext)

            if(exitOnReturn) {
              log.info("Stream processing started. Press RETURN to stop...")

              // Support stop on key press
              keyPressFuture = Some(Future({
                StdIn.readLine()
                log.info("Stop requested via StdIn.")
                worker.stopWork()
              })(worker.getExecutionContext))
            }


          case Failure(ex) =>
            log.error("Failed to initialize application", ex)
            worker.shutdown()
            System.exit(1)
        }
      case None =>
        System.exit(-1)

    }
  }

  protected def buildWorker(config: T): AsyncStreamWorker[_]

  protected def buildConfig(typesafeConfig: Config): Try[T]

  protected def onComplete(): Unit = {
    log.info("Finished.")
  }

  protected def exitOnReturn: Boolean = true


  private def loadConfig(): Option[T] = {

    log.debug("Loading configuration..")
    val configTry = buildConfig(ConfigFactory.load())

    if(configTry.isSuccess){
      Some(configTry.get)
    } else {
      log.error("Error loading configuration", configTry.failed.get)
      None
    }

  }

}
