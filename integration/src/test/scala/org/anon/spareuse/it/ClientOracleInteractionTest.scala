package org.anon.spareuse.it

import com.dimafeng.testcontainers.GenericContainer.DockerImage
import com.dimafeng.testcontainers.lifecycle.and
import com.dimafeng.testcontainers.{GenericContainer, PostgreSQLContainer, RabbitMQContainer}
import org.scalatest.flatspec.AnyFlatSpec
import org.slf4j.{Logger, LoggerFactory}
import com.dimafeng.testcontainers.scalatest.TestContainersForAll
import org.testcontainers.Testcontainers
import org.testcontainers.containers.wait.strategy.LogMessageWaitStrategy

class ClientOracleInteractionTest extends AnyFlatSpec with TestContainersForAll {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  // Define set of containers to use for this test - PostgreSQL, Rabbit, Generic(API)
  override type Containers = PostgreSQLContainer and RabbitMQContainer and GenericContainer



  override def startContainers(): Containers = {
    log.info(s"Starting containers ...")
    val pgContainer = buildPgContainer
    val mqContainer = buildMqContainer

    pgContainer.start()
    log.info(s"Successfully started PostgreSQL container: ${pgContainer.containerId}")
    mqContainer.start()
    log.info(s"Successfully started RabbitMQ container: ${mqContainer.containerId}")


    val mappedPgPortOnHost = pgContainer.mappedPort(5432)
    val mappedMqPortOnHost = mqContainer.mappedPort(5672)
    Testcontainers.exposeHostPorts(mappedPgPortOnHost, mappedMqPortOnHost)

    val apiWaitStrategy = new LogMessageWaitStrategy()
      .withRegEx(".*(WebApi online at).*")

    val apiContainerDef = GenericContainer.Def(DockerImage(Left("spar-webapi:latest")), env = Map(
      "SPARRI_MQ_USER" -> mqContainer.adminUsername,
      "SPARRI_MQ_PASS" -> mqContainer.adminPassword,
      "SPARRI_MQ_HOST" -> "host.testcontainers.internal",
      "SPARRI_MQ_PORT" -> mappedMqPortOnHost.toString,
      "SPARRI_DB_USER" -> pgUserName,
      "SPARRI_DB_PASS" -> pgPass,
      "SPARRI_DB_URL" -> s"jdbc:postgresql://host.testcontainers.internal:$mappedPgPortOnHost/",
    ), exposedPorts = Seq(9090), waitStrategy = apiWaitStrategy)

    val apiContainer = apiContainerDef.start()
    log.info(s"Successfully started SPARRI API container: ${apiContainer.containerId}")

    pgContainer and mqContainer and apiContainer
  }

  "The client analysis" should "fail when requirements are not met" in withContainers {
    case postgres and rabbitMq and api =>
      val apiHost = "localhost"
      val apiPort = api.mappedPort(9090)
  }

}
