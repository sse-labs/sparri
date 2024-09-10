package org.anon.spareuse

import com.dimafeng.testcontainers.{PostgreSQLContainer, RabbitMQContainer}
import org.testcontainers.utility.DockerImageName

package object it {

  // PostgreSQL Docker definitions for SPARRI integration tests
  val pgImage = "postgres:14.2"
  val pgUserName = "it-test"
  val pgPass = "sparri-it"

  def buildPgContainer = new PostgreSQLContainer(
    dockerImageNameOverride = Some(DockerImageName.parse(pgImage)),
    databaseName = Some("spa-results"),
    pgUsername = Some(pgUserName),
    pgPassword = Some(pgPass)
  )

  // RabbitMQ Docker definitions for SPARRI integration tests
  val mqImage = "rabbitmq:3.10"

  def buildMqContainer = new RabbitMQContainer(
    dockerImageName = DockerImageName.parse(mqImage)
  )
}
