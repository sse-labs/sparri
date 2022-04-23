package de.tudo.sse.classfilefeatures.webapi

import de.tudo.sse.classfilefeatures.common.rabbitmq.MqConnectionConfiguration
import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.PostgreSqlConnectionConfiguration

class Configuration extends PostgreSqlConnectionConfiguration
  with MqConnectionConfiguration{

  val serverHost: String = "localhost"
  val serverPort: Int = 33449

  val enqueuePackageIdentifiersOnNotFound: Boolean = true

  override val postgreSqlUrl: String = "<CHANGEME>"
  override val postgresUsername: String = "postgres"
  override val postgresPassword: String = "<CHANGEME>"

  override val mqUsername: String = "<CHANGEME>"
  override val mqPassword: String = "<CHANGEME>"
  override val mqHost: String = "<CHANGEME>"
  override val mqPort: Int = 8080
  override val mqQueueName: String = "library-identifiers"
}
