package de.tudo.sse.classfilefeatures.webapi

import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.PostgreSqlConnectionConfiguration

class Configuration extends PostgreSqlConnectionConfiguration{

  val serverHost: String = "localhost"
  val serverPort: Int = 33449

  override val postgreSqlUrl: String = "<CHANGEME>"
  override val postgresUsername: String = "<CHANGEME>"
  override val postgresPassword: String = "<CHANGEME>"
}
