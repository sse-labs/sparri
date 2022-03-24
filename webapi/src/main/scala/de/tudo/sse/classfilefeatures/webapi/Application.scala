package de.tudo.sse.classfilefeatures.webapi

import akka.actor.ActorSystem
import de.tudo.sse.classfilefeatures.webapi.server.ApiServer
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Application {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {

    val theSystem = ActorSystem("cf-webapi-system")
    implicit val executionContext: ExecutionContext = theSystem.dispatcher

    val configuration = new Configuration
    val webapiServer = new ApiServer()(theSystem)

    Try(webapiServer.startServer(configuration.serverHost, configuration.serverPort)) match {
      case Success(bindingFuture) =>
        log.info(s"WebApi online at ${configuration.serverHost}:${configuration.serverPort}. Press any key to shutdown..")

        StdIn.readLine()

        bindingFuture
          .flatMap(_.unbind())
          .onComplete{ _ =>
            webapiServer.shutdown()
            theSystem.terminate()
          }

      case Failure(ex) =>
        log.error(s"Failed to start server at ${configuration.serverHost}:${configuration.serverPort}", ex)
        webapiServer.shutdown()
        theSystem.terminate()
    }
  }

}
