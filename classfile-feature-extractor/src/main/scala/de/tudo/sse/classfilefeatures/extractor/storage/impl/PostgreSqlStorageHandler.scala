package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.extractor.model.LibraryClassfileFeatureModel
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler
import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlStorageHandler.buildConnection

import java.sql.{Connection, DriverManager}
import java.util.Properties
import scala.util.{Success, Try}

class PostgreSqlStorageHandler(config: PostgreSqlConnectionConfiguration) extends ClassfileFeatureStorageHandler {

  private lazy val connection = buildConnection(config.postgreSqlUrl, config.postgresUsername, config.postgresPassword)

  override def verifyConnectivity(): Unit = {

    // connection is a lazy val, so any errors while building it will be thrown here!
    if(!connection.isValid(30)){
      throw new IllegalStateException("PostgreSQL connection not valid.")
    }
  }

  override def isLibraryPresent(libraryIdentifier: String): Boolean = false // TODO: Implement

  override def storeLibraryFeatureModel(model: LibraryClassfileFeatureModel): Try[Unit] = {
    log.info(s"Storing ${model.libraryIdentifier}")
    Success()
  } //TODO: Implement

  override def shutdown(): Unit = {} //TODO: IMPLEMENT
}

object PostgreSqlStorageHandler {

  def buildConnection(url: String, username: String, password: String): Connection = {
    Class.forName("org.postgresql.Driver")

    val props = new Properties()
    props.setProperty("user", username)
    props.setProperty("password", password)

    DriverManager.getConnection(url, props)
  }

}

trait PostgreSqlConnectionConfiguration {

  val postgreSqlUrl: String
  val postgresUsername: String
  val postgresPassword: String

}
