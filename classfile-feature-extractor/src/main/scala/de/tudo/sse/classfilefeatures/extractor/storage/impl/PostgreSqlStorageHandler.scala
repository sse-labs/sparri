package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.extractor.model.LibraryClassfileFeatureModel
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler
import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlStorageHandler.buildConnection

import java.sql.{Connection, DriverManager, Statement}
import java.util.Properties
import java.util.concurrent.locks.ReentrantLock
import scala.util.{Failure, Success, Try}

class PostgreSqlStorageHandler(config: PostgreSqlConnectionConfiguration) extends ClassfileFeatureStorageHandler {

  private val libraryIndexRWLock: ReentrantLock = new ReentrantLock()

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

    // ATOMIC TABLE DEFINITIONS
    stmt.addBatch(PostgreSqlTables.libraryTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.versionNumberTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.accessFlagsTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.classFileTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.fieldDefinitionTable.tableDefinitionSql)

    // RELATION TABLE DEFINITIONS
    stmt.addBatch(PostgreSqlTables.libraryToVersionsTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.classFilesToVersionsTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.classFilesToFlagsTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.fieldDefinitionsToVersionsTable.tableDefinitionSql)

    stmt.addBatch(PostgreSqlTables.fieldDefinitionsToFlagsTable.tableDefinitionSql)


    if(stmt.executeBatch().contains(Statement.EXECUTE_FAILED)){
      stmt.close()
      connection.rollback()
      throw new Exception("Failed to create tables in database")
    }

    stmt.close()
    connection.commit()
  }

  override def isLibraryPresent(libraryIdentifier: String): Boolean = {
    val queryStmt = connection.prepareStatement("SELECT LibraryIdentifier FROM Libraries WHERE Libraries.LibraryIdentifier = ?;")
    queryStmt.setString(1, libraryIdentifier)
    val result = queryStmt.executeQuery()
    val libraryPresent = result.next()

    result.close()
    queryStmt.close()

    libraryPresent
  }

  override def storeLibraryFeatureModel(model: LibraryClassfileFeatureModel): Try[Unit] = {
    //TODO: Tackle performance issues, use more efficient ids

    // Threadsafe way to check whether library is present. Only proceed if it was not present before.
    libraryIndexRWLock.lock()

    var libraryWasPresent = false

    try {
      libraryWasPresent = isLibraryPresent(model.libraryIdentifier)

      if(!libraryWasPresent){
        connection.rollback()

        // Insert library into index table before releasing the lock
        val prepStmt = PostgreSqlTables.libraryTable.buildInsertStmt(connection, ignoreConflicts = false)
        prepStmt.setString(1, model.libraryIdentifier)
        prepStmt.executeUpdate()

        prepStmt.close()
        connection.commit()
      }
    } finally {
      libraryIndexRWLock.unlock()
    }


    if(libraryWasPresent){
      // If the library was present before, we don't have to do anything
      Success()
    } else {
      Try {
        //If it was not present before, we have to store all data

        // Insert version numbers into table. Ignore conflicts, as we do not want duplicate version numbers in the table
        val versionPrepStmt = PostgreSqlTables.versionNumberTable.buildInsertStmt(connection, ignoreConflicts = true)

        model.releases.foreach { v =>
          versionPrepStmt.setString(1, v)
          versionPrepStmt.addBatch()
        }
        versionPrepStmt.executeBatch()
        versionPrepStmt.close()
        connection.commit()

        // Insert all flags from this model. Ignore conflicts, as we do not want duplicate flags in the table
        val allFlags = model.allClassfileModels.flatMap(_.flagsEvolution.valueToReleasesMap.keySet).distinct ++
          model.allClassfileModels.flatMap(_.fieldDefinitionEvolutions).flatMap(_.flagsEvolution.valueToReleasesMap.keySet).distinct ++
          model.allClassfileModels.flatMap(_.methodEvolutions).flatMap(_.flagsEvolution.valueToReleasesMap.keySet).distinct

        val flagsPrepStmt = PostgreSqlTables.accessFlagsTable.buildInsertStmt(connection, ignoreConflicts = true)

        allFlags.foreach { f =>
          flagsPrepStmt.setInt(1, f)
          flagsPrepStmt.addBatch()
        }
        flagsPrepStmt.executeBatch()
        flagsPrepStmt.close()
        connection.commit()

        // Insert all classfiles
        val cfPrepStmt = PostgreSqlTables.classFileTable.buildInsertStmt(connection, ignoreConflicts = false)

        model.allClassfileModels.foreach { cfm =>
          cfPrepStmt.setString(1, cfm.identifier)
          cfPrepStmt.setString(2, model.libraryIdentifier)
          cfPrepStmt.addBatch()
        }
        cfPrepStmt.executeBatch()
        cfPrepStmt.close()
        connection.commit()

        // Insert all field definitions
        val fieldDefPrepStmt = PostgreSqlTables.fieldDefinitionTable.buildInsertStmt(connection, ignoreConflicts = false)

        model.allClassfileModels.flatMap(m => m.fieldDefinitionEvolutions.map(f => (m.classFileThisTypeFqn, f))).foreach{ lfdm =>
          fieldDefPrepStmt.setString(1, lfdm._1)
          fieldDefPrepStmt.setString(2, model.libraryIdentifier)
          fieldDefPrepStmt.setString(3, lfdm._2.fieldName)
          fieldDefPrepStmt.setString(4, lfdm._2.fieldTypeJvmName)
          fieldDefPrepStmt.addBatch()
        }
        fieldDefPrepStmt.executeBatch()
        fieldDefPrepStmt.close()
        connection.commit()

        // Insert all field def to access flags relations
        val fieldDefToFlagsPrepStmt = PostgreSqlTables.fieldDefinitionsToFlagsTable.buildInsertStmt(connection, ignoreConflicts = false)

        model.allClassfileModels.flatMap(m => m.fieldDefinitionEvolutions.map(f => (m.classFileThisTypeFqn, f))).foreach { lfdm =>

          lfdm._2.flagsEvolution.getReleaseToValueTuples.foreach { ft =>
            fieldDefToFlagsPrepStmt.setString(1, lfdm._1)
            fieldDefToFlagsPrepStmt.setString(2, model.libraryIdentifier)
            fieldDefToFlagsPrepStmt.setString(3, lfdm._2.fieldName)
            fieldDefToFlagsPrepStmt.setString(4, lfdm._2.fieldTypeJvmName)
            fieldDefToFlagsPrepStmt.setInt(5, ft._2)
            fieldDefToFlagsPrepStmt.setString(6, ft._1)
            fieldDefToFlagsPrepStmt.addBatch()
          }

        }
        fieldDefToFlagsPrepStmt.executeBatch()
        fieldDefToFlagsPrepStmt.close()
        connection.commit()


        // Insert all library-to-version connections
        val libToVersionPrepStmt = PostgreSqlTables.libraryToVersionsTable.buildInsertStmt(connection, ignoreConflicts = false)

        model.releases.foreach { v =>
          libToVersionPrepStmt.setString(1, model.libraryIdentifier)
          libToVersionPrepStmt.setString(2, v)
          libToVersionPrepStmt.addBatch()
        }
        libToVersionPrepStmt.executeBatch()
        libToVersionPrepStmt.close()
        connection.commit()


        // Insert all classfile-to-version, classfile-to-fielddef and classfile-to-flags relations
        val cfToVersionPrepStmt = PostgreSqlTables.classFilesToVersionsTable.buildInsertStmt(connection, ignoreConflicts = false)
        val cfToFlagsPrepStmt = PostgreSqlTables.classFilesToFlagsTable.buildInsertStmt(connection, ignoreConflicts = false)
        val cfToFieldDefPrepStmt = PostgreSqlTables.fieldDefinitionsToVersionsTable.buildInsertStmt(connection, ignoreConflicts = false)

        model.allClassfileModels.foreach { cfm =>

          cfm.allActiveReleases.foreach { v =>
            cfToVersionPrepStmt.setString(1, cfm.identifier)
            cfToVersionPrepStmt.setString(2, model.libraryIdentifier)
            cfToVersionPrepStmt.setString(3, v)
            cfToVersionPrepStmt.addBatch()
          }

          cfm.flagsEvolution.getReleaseToValueTuples.foreach { tuple =>
            cfToFlagsPrepStmt.setString(1, cfm.identifier)
            cfToFlagsPrepStmt.setString(2, model.libraryIdentifier)
            cfToFlagsPrepStmt.setInt(3, tuple._2)
            cfToFlagsPrepStmt.setString(4, tuple._1)
            cfToFlagsPrepStmt.addBatch()
          }

          cfm.fieldDefinitionEvolutions.foreach{ lfdm =>
            lfdm.allActiveReleases.foreach { v =>
              cfToFieldDefPrepStmt.setString(1, cfm.identifier)
              cfToFieldDefPrepStmt.setString(2, model.libraryIdentifier)
              cfToFieldDefPrepStmt.setString(3, lfdm.fieldName)
              cfToFieldDefPrepStmt.setString(4, lfdm.fieldTypeJvmName)
              cfToFieldDefPrepStmt.setString(5, v)
              cfToFieldDefPrepStmt.addBatch()
            }
          }

          cfToVersionPrepStmt.executeBatch()
          cfToFlagsPrepStmt.executeBatch()
          cfToFieldDefPrepStmt.executeBatch()
        }

        cfToVersionPrepStmt.close()
        cfToFlagsPrepStmt.close()
        cfToFieldDefPrepStmt.close()
        connection.commit()


        //TODO: Complete
      } match {
        case f@Failure(_) =>
          connection.rollback()
          f
        case s@Success(_) => s
      }
    }

  }

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
