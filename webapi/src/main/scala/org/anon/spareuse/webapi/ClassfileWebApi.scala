package org.anon.spareuse.webapi

import akka.actor.ActorSystem
import akka.stream.Materializer
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.storage.postgresql.PostgresDataAccessor
import org.anon.spareuse.core.utils.rabbitmq.MqMessageWriter
import org.anon.spareuse.execution.analyses.impl.cg.JreModelLoader
import org.anon.spareuse.webapi.core.{OracleResolutionRequestHandler, RequestHandler}
import org.anon.spareuse.webapi.server.ApiServer
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

class ClassfileWebApi(private val configuration: WebapiConfig) {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private final val theSystem: ActorSystem = ActorSystem("cf-webapi-system")
  private final val materializer: Materializer = Materializer(theSystem)

  private[webapi] lazy val dataAccessor: DataAccessor = new PostgresDataAccessor()(materializer.executionContext)
  private[webapi] lazy val requestHandler: RequestHandler = new RequestHandler(configuration, dataAccessor)(materializer.executionContext)
  private[webapi] lazy val oracleRequestHandler: OracleResolutionRequestHandler = new OracleResolutionRequestHandler(dataAccessor)(materializer.executionContext)
  private[webapi] lazy val server: ApiServer = new ApiServer(requestHandler, oracleRequestHandler)(theSystem)

  private var dbInitialized = false

  def initialize(): Boolean = {

    JreModelLoader.indexJreData(configuration.jreDataDir)

    // Cascaded lazy-vals will throw any db related error here.
    Try(dataAccessor.initialize()) match {
      case Success(_) =>
        dbInitialized = true

        Try {
          val analysisWriter = new MqMessageWriter(configuration.asAnalysisQueuePublishConfig)
          analysisWriter.initialize()
          analysisWriter.shutdown()
          val entityWriter = new MqMessageWriter(configuration.asMinerQueuePublishConfig)
          entityWriter.initialize()
          entityWriter.shutdown()
        } match {
          case Success(_) => true
          case Failure(ex) =>
            log.error("Failed to initialize message queues", ex)
            false
        }

      case Failure(ex) =>
        log.error("Error during initialization: Failed to establish database connection", ex)
        false
    }
  }

  def runUntilButtonPressed(): Unit = {
    Try(server.startServer(configuration.bindHost, configuration.bindPort)) match {
      case Success(bindingFuture) =>

        implicit val dispatcher: ExecutionContext = theSystem.dispatcher

        log.info(s"WebApi online at ${configuration.bindHost}:${configuration.bindPort}. Press any key to shutdown..")

        StdIn.readLine()

        bindingFuture
          .flatMap { _.unbind() }
          .onComplete { _ => shutdown() }

      case Failure(ex) =>
        log.error(s"Failed to start server at ${configuration.bindHost}:${configuration.bindPort}", ex)
        shutdown()
    }
  }

  def run(): Unit = {
    Try(server.startServer(configuration.bindHost, configuration.bindPort)) match {
      case Success(bindingFuture) =>

        implicit val dispatcher: ExecutionContext = theSystem.dispatcher

        log.info(s"WebApi online at ${configuration.bindHost}:${configuration.bindPort}")

      case Failure(ex) =>
        log.error(s"Failed to start server at ${configuration.bindHost}:${configuration.bindPort}", ex)
        shutdown()
    }
  }

  def shutdown(): Unit = {
    log.info("Shutting down WebApi ..")

    // Shutdown Http connection pool
    server.shutdown()

    // Shutdown JDBC connection
    if(dbInitialized) dataAccessor.shutdown()

    materializer.shutdown()

    // Shutdown actor system
    Await.ready(theSystem.terminate(), 10.seconds)

    log.info("Shutdown complete.")
  }



}
