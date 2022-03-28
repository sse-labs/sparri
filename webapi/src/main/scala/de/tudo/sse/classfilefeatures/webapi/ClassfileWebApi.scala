package de.tudo.sse.classfilefeatures.webapi

import akka.actor.ActorSystem
import de.tudo.sse.classfilefeatures.webapi.core.RequestHandler
import de.tudo.sse.classfilefeatures.webapi.server.ApiServer
import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor
import de.tudo.sse.classfilefeatures.webapi.storage.impl.PostgreSqlDataAccessor
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

class ClassfileWebApi {

  private final val log: Logger = LoggerFactory.getLogger(getClass)
  private final val configuration: Configuration = new Configuration

  private final val theSystem: ActorSystem = ActorSystem("cf-webapi-system")

  private[webapi] lazy val dataAccessor: ClassfileDataAccessor = new PostgreSqlDataAccessor(configuration)
  private[webapi] lazy val requestHandler: RequestHandler = new RequestHandler(dataAccessor)
  private[webapi] lazy val server: ApiServer = new ApiServer(requestHandler)(theSystem)

  private var dbInitialized = false

  def initialize(): Boolean = {
    // Cascaded lazy-vals will throw any db related error here.
    Try(dataAccessor.verifyConnectivity()) match {
      case Success(_) =>
        dbInitialized = true
        true
      case Failure(ex) =>
        log.error("Error during initialization: Failed to establish database connection", ex)
        false
    }
  }

  def runUntilButtonPressed(): Unit = {
    Try(server.startServer(configuration.serverHost, configuration.serverPort)) match {
      case Success(bindingFuture) =>

        implicit val dispatcher: ExecutionContext = theSystem.dispatcher

        log.info(s"WebApi online at ${configuration.serverHost}:${configuration.serverPort}. Press any key to shutdown..")

        StdIn.readLine()

        bindingFuture
          .flatMap { _.unbind() }
          .onComplete { _ => shutdown() }

      case Failure(ex) =>
        log.error(s"Failed to start server at ${configuration.serverHost}:${configuration.serverPort}", ex)
        shutdown()
    }
  }

  def shutdown(): Unit = {
    log.info("Shutting down WebApi ..")

    // Shutdown Http connection pool
    server.shutdown()

    // Shutdown JDBC connection
    if(dbInitialized) dataAccessor.shutdown()

    // Shutdown actor system
    Await.ready(theSystem.terminate(), 10.seconds)

    log.info("Shutdown complete.")
  }



}