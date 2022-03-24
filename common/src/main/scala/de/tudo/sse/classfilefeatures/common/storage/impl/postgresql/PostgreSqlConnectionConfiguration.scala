package de.tudo.sse.classfilefeatures.common.storage.impl.postgresql

trait PostgreSqlConnectionConfiguration {

  val postgreSqlUrl: String
  val postgresUsername: String
  val postgresPassword: String

}
