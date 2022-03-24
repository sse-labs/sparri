package de.tudo.sse.classfilefeatures.common.storage.impl.postgresql

import java.sql.{Connection, DriverManager}
import java.util.Properties

trait PostgreSqlConnectivity {

  protected lazy val connection: Connection = buildConnection()

  protected def buildConnection(): Connection = {
    Class.forName("org.postgresql.Driver")

    val props = new Properties()
    props.setProperty("user", connectionConfiguration.postgresUsername)
    props.setProperty("password", connectionConfiguration.postgresPassword)

    DriverManager.getConnection(connectionConfiguration.postgreSqlUrl, props)
  }

  protected val connectionConfiguration: PostgreSqlConnectionConfiguration

}
