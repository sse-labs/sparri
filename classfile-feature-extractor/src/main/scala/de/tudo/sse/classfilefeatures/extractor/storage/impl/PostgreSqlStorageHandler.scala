package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.extractor.model.LibraryClassfileFeatureModel
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler
import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlStorageHandler.buildConnection

import java.sql.{Connection, DriverManager, Statement}
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

  override def initialize(): Unit = {
    connection.setAutoCommit(false) // We want manual transactions!
    connection.rollback()

    val stmt = connection.createStatement()

    stmt.addBatch("CREATE TABLE IF NOT EXISTS Libraries( " +
      "LibraryIdentifier VARCHAR(255) NOT NULL, " +
      "CONSTRAINT PK_Libraries PRIMARY KEY (LibraryIdentifier) " +
      ");")

    stmt.addBatch("CREATE TABLE IF NOT EXISTS Classfiles( " +
      "ThisType VARCHAR(1024) NOT NULL, " +
      "Library VARCHAR(255) NOT NULL, " +
      "CONSTRAINT FK_Cfs_Libraries FOREIGN KEY (Library) REFERENCES Libraries(LibraryIdentifier), " +
      "CONSTRAINT PK_Cfs PRIMARY KEY (ThisType, Library) " +
      ");")

    if(stmt.executeBatch().contains(Statement.EXECUTE_FAILED)){
      connection.rollback()
      throw new Exception("Failed to create tables in database")
    }

    connection.commit()
  }

  override def isLibraryPresent(libraryIdentifier: String): Boolean = {
    false
  } // TODO: Implement

  override def storeLibraryFeatureModel(model: LibraryClassfileFeatureModel): Try[Unit] = Try {
    log.info(s"Storing ${model.libraryIdentifier}")

    connection.rollback()

    val prepStmt = connection.prepareStatement("INSERT INTO Libraries VALUES ( ? );")
    prepStmt.setString(1, model.libraryIdentifier)
    prepStmt.executeUpdate()

    connection.commit()

    val cfPrepStmt = connection.prepareStatement("INSERT INTO Classfiles VALUES( ?, ?);")

    model.allClassfileModels.foreach { cfm =>
      cfPrepStmt.setString(1, cfm.identifier)
      cfPrepStmt.setString(2, model.libraryIdentifier)
      cfPrepStmt.executeUpdate()
      cfPrepStmt.clearParameters()
    }

    connection.commit()
  } //TODO: Implement

  override def shutdown(): Unit = {
    connection.close()
  }
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
