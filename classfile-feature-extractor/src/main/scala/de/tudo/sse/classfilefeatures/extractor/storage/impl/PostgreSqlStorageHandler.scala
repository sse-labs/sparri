package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.extractor.model.{LibraryClassfileFeatureModel, ValueEvolution}
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler
import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlStorageHandler.buildConnection

import java.sql.{Connection, DriverManager, PreparedStatement, Statement}
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
    connection.setAutoCommit(false)

    val stmt = connection.createStatement()

    PostgreSqlTables.allEntityTables.foreach { table =>
      stmt.addBatch(table.tableDefinitionSql)
    }

    stmt.addBatch("CREATE INDEX idx_field_signature ON " + PostgreSqlTables.fieldSignatureTable.tableName + " (FieldName, FieldType)")

    PostgreSqlTables.allRelationTables.foreach { table =>
      stmt.addBatch(table.tableDefinitionSql)
    }

    if(stmt.executeBatch().contains(Statement.EXECUTE_FAILED)){
      stmt.close()
      throw new Exception("Failed to create tables in database")
    }

    stmt.close()
    connection.commit()
  }

  override def isLibraryPresent(libraryIdentifier: String): Boolean = {
    libraryIndexRWLock.lock()

    var libraryPresent = false

    try{
      val queryStmt = connection.prepareStatement("SELECT LibraryName FROM Libraries WHERE Libraries.LibraryName = ?;")
      queryStmt.setString(1, libraryIdentifier)
      val result = queryStmt.executeQuery()
      libraryPresent = result.next()

      result.close()
      queryStmt.close()
    } finally {
      libraryIndexRWLock.unlock()
    }


    libraryPresent
  }

  override def storeLibraryFeatureModel(model: LibraryClassfileFeatureModel): Try[Unit] = {
    //TODO: Tackle performance issues, use more efficient ids

    // Threadsafe way to check whether library is present. Only proceed if it was not present before.
    libraryIndexRWLock.lock()

    var libraryWasPresent = false
    var libraryId = -1

    try {
      libraryWasPresent = isLibraryPresent(model.libraryIdentifier)

      if(!libraryWasPresent){

        // Insert library into index table before releasing the lock
        val prepStmt = connection.prepareStatement(PostgreSqlTables.libraryTable.buildInsertSql(ignoreConflicts = false),
          Statement.RETURN_GENERATED_KEYS)

        prepStmt.setString(1, model.libraryIdentifier)
        prepStmt.executeUpdate()

        val result = prepStmt.getGeneratedKeys

        if(result.next()){
          libraryId = result.getInt(1)
        }

        result.close()
        prepStmt.close()
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

        // Adds all versions to table and stores id mapping
        val versionToIdMap = model.releases.map(r => (r, assertVersionPresentAndGetId(connection, r))).toMap
        connection.commit()

        // Adds all flags to table. No need to store id mapping, flag value is PK
        val allFlagsInModel = model.allClassfileModels.flatMap(_.flagsEvolution.valueToReleasesMap.keySet)++
          model.allClassfileModels.flatMap(_.fieldDefinitionEvolutions).flatMap(_.flagsEvolution.valueToReleasesMap.keySet) ++
          model.allClassfileModels.flatMap(_.methodEvolutions).flatMap(_.flagsEvolution.valueToReleasesMap.keySet)
        connection.commit()

        val flagsPrepStmt = PostgreSqlTables.accessFlagsTable.buildInsertStmt(connection, ignoreConflicts = true)

        allFlagsInModel.toSet.foreach { f: Int =>
          flagsPrepStmt.setInt(1, f)
          flagsPrepStmt.addBatch()
        }
        flagsPrepStmt.executeBatch()
        flagsPrepStmt.close()
        connection.commit()

        log.info(s"[${model.libraryIdentifier}] Start building field index...") //TODO: Speedup this index
        // Assert that all field signature are present and build an id lookup
        val allFieldSignaturesInModel: Set[(String, String)] =
          (model.allClassfileModels.flatMap(_.fieldDefinitionEvolutions).map(lfdm => (lfdm.fieldName, lfdm.fieldTypeJvmName)) ++
          model.allClassfileModels.flatMap(_.methodEvolutions).flatMap(_.fieldAccessEvolutions).map( lfaim => (lfaim.fieldName, lfaim.fieldTypeJvmName))).toSet

        val fieldSignatureToIdMap = allFieldSignaturesInModel.map( sig => (sig._1 + sig._2, assertFieldSignaturePresentAndGetId(connection, sig._1, sig._2) )).toMap
        connection.commit()
        log.info(s"[${model.libraryIdentifier}] Done building field index.")


        val libraryToVersionInsertStmt = PostgreSqlTables.librariesToVersionsTable.buildInsertStmt(connection, ignoreConflicts = false)
        model.releases.foreach { v =>
          libraryToVersionInsertStmt.setInt(1, libraryId)
          libraryToVersionInsertStmt.setInt(2, versionToIdMap(v))
          libraryToVersionInsertStmt.addBatch()
        }

        libraryToVersionInsertStmt.executeBatch()
        libraryToVersionInsertStmt.close()
        connection.commit()


        val classFileInsertStmt = connection.prepareStatement(PostgreSqlTables.classFileTable.buildInsertSql(ignoreConflicts = false),
          Statement.RETURN_GENERATED_KEYS)

        val classFileToVersionInsertStmt = connection.prepareStatement(PostgreSqlTables.classFilesToVersionsTable.buildInsertSql(ignoreConflicts = false))
        val classFileToFlagsInsertStmt = connection.prepareStatement(PostgreSqlTables.classFileFlagValueTable.buildInsertSql(ignoreConflicts = false))

        val methodInsertStmt = connection.prepareStatement(PostgreSqlTables.methodTable.buildInsertSql(ignoreConflicts = false),
          Statement.RETURN_GENERATED_KEYS)
        val methodToVersionInsertStmt = connection.prepareStatement(PostgreSqlTables.methodsToVersionsTable.buildInsertSql(ignoreConflicts = false))
        val methodToFlagsInsertStmt = connection.prepareStatement(PostgreSqlTables.methodFlagValueTable.buildInsertSql(ignoreConflicts = false))
        val methodToMaxStackInsertStmt = connection.prepareStatement(PostgreSqlTables.methodMaxStackValueTable.buildInsertSql(ignoreConflicts = false))
        val methodToMaxLocalsInsertStmt = connection.prepareStatement(PostgreSqlTables.methodMaxLocalsValueTable.buildInsertSql(ignoreConflicts = false))

        val fieldDefinitionInsertStmt = connection.prepareStatement(PostgreSqlTables.fieldDefinitionTable.buildInsertSql(ignoreConflicts = false),
          Statement.RETURN_GENERATED_KEYS)

        val fieldDefToVersionInsertStmt = connection.prepareStatement(PostgreSqlTables.fieldDefinitionsToVersionsTable.buildInsertSql(ignoreConflicts = false))
        val fieldDefToFlagsInsertStmt = connection.prepareStatement(PostgreSqlTables.fieldDefinitionFlagValueTable.buildInsertSql(ignoreConflicts = false))


        model.allClassfileModels.foreach { lcfm =>

          val cfDefaultFlags = lcfm.flagsEvolution.getDefaultValue

          // Store classfile itself
          classFileInsertStmt.setInt(1, libraryId)
          classFileInsertStmt.setString(2, lcfm.classFileThisTypeFqn)
          classFileInsertStmt.setInt(3, cfDefaultFlags)

          classFileInsertStmt.executeUpdate()

          val rs = classFileInsertStmt.getGeneratedKeys

          val classFileId = if(rs.next()) rs.getInt(1)
            else throw new Exception(s"Failed to store classfile ${lcfm.classFileThisTypeFqn}")
          connection.commit()

          // Store all methods of classfile
          lcfm.methodEvolutions.foreach { lmm =>

            val methodDefaultFlags = lmm.flagsEvolution.getDefaultValue
            // For MaxStack and MaxLocals there may not be any value at all (in case of abstract methods)
            val methodDefaultMaxStack = lmm.maxStackEvolution.getDefaultValueOpt
            val methodDefaultMaxLocals = lmm.maxLocalsEvolution.getDefaultValueOpt

            methodInsertStmt.setInt(1, classFileId)
            methodInsertStmt.setString(2, lmm.methodName)
            methodInsertStmt.setString(3, lmm.jvmMethodDescriptor)
            methodInsertStmt.setInt(4, methodDefaultFlags)

            if(methodDefaultMaxStack.isDefined) methodInsertStmt.setInt(5, methodDefaultMaxStack.get)
            else methodInsertStmt.setNull(5, java.sql.Types.INTEGER)

            if(methodDefaultMaxLocals.isDefined) methodInsertStmt.setInt(6, methodDefaultMaxLocals.get)
            else methodInsertStmt.setNull(6, java.sql.Types.INTEGER)

            methodInsertStmt.setBoolean(7, lmm.hasBody)

            methodInsertStmt.executeUpdate()
            val mrs = methodInsertStmt.getGeneratedKeys

            val methodId = if(mrs.next())  mrs.getInt(1)
              else throw new Exception(s"Failed to store methods for classfile ${lcfm.classFileThisTypeFqn}")
            connection.commit()

            // Store exceptions to default flag for method
            storeExceptionsToDefaultValue(lmm.flagsEvolution, methodToFlagsInsertStmt, methodId, versionToIdMap)

            if(methodDefaultMaxStack.isDefined)
              storeExceptionsToDefaultValue(lmm.maxStackEvolution, methodToMaxStackInsertStmt, methodId, versionToIdMap)

            if(methodDefaultMaxLocals.isDefined)
              storeExceptionsToDefaultValue(lmm.maxLocalsEvolution, methodToMaxLocalsInsertStmt, methodId, versionToIdMap)


            // Store method-to-version relation
            lmm.allActiveReleases.foreach { v =>

              methodToVersionInsertStmt.setInt(1, methodId)
              methodToVersionInsertStmt.setInt(2, versionToIdMap(v))
              methodToVersionInsertStmt.addBatch()
            }

            methodToVersionInsertStmt.executeBatch()
          }

          // Store exceptions from default flag for classfile
          storeExceptionsToDefaultValue(lcfm.flagsEvolution, classFileToFlagsInsertStmt, classFileId, versionToIdMap)

          // Store classfile field definitions
          lcfm.fieldDefinitionEvolutions.foreach { lfdm =>

            val fdDefaultFlags = lfdm.flagsEvolution.getDefaultValue

            // Store field definition itself
            fieldDefinitionInsertStmt.setInt(1, classFileId)
            fieldDefinitionInsertStmt.setInt(2, fieldSignatureToIdMap(lfdm.fieldName + lfdm.fieldTypeJvmName))
            fieldDefinitionInsertStmt.setInt(3, fdDefaultFlags)

            fieldDefinitionInsertStmt.executeUpdate()

            val fdrs = fieldDefinitionInsertStmt.getGeneratedKeys

            val fieldDefinitionId = if(fdrs.next()) fdrs.getInt(1)
              else throw new Exception(s"Failed to store classfile field definition for ${lcfm.classFileThisTypeFqn}")

            // Store exceptions from default flag for field definition
            storeExceptionsToDefaultValue(lfdm.flagsEvolution, fieldDefToFlagsInsertStmt, fieldDefinitionId, versionToIdMap)

            // Store fielddef-to-version relations
            lfdm.allActiveReleases.foreach { v =>
              fieldDefToVersionInsertStmt.setInt(1, fieldDefinitionId)
              fieldDefToVersionInsertStmt.setInt(2, versionToIdMap(v))
              fieldDefToVersionInsertStmt.addBatch()
            }

            fieldDefToVersionInsertStmt.executeBatch()
          }
          connection.commit()

          // Store classfile-to-version relations
          lcfm.allActiveReleases.foreach { v =>
            classFileToVersionInsertStmt.setInt(1, classFileId)
            classFileToVersionInsertStmt.setInt(2, versionToIdMap(v))
            classFileToVersionInsertStmt.addBatch()
          }

          classFileToVersionInsertStmt.executeBatch()
          connection.commit()
        }

        classFileInsertStmt.close()
        classFileToVersionInsertStmt.close()
        classFileToFlagsInsertStmt.close()
        methodInsertStmt.close()
        methodToVersionInsertStmt.close()
        fieldDefinitionInsertStmt.close()
        fieldDefToVersionInsertStmt.close()
        fieldDefToFlagsInsertStmt.close()

        /* Insert version numbers into table. Ignore conflicts, as we do not want duplicate version numbers in the table
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
        cfToFieldDefPrepStmt.close()*/


        //TODO: Complete
      }
    }

  }

  override def shutdown(): Unit = {
    connection.close()
  }

  private def assertFieldSignaturePresentAndGetId(connection: Connection, fieldName: String, fieldType: String): Int = {
    val checkIdStmt =
      connection.prepareStatement("SELECT Id FROM " + PostgreSqlTables.fieldSignatureTable.tableName + " WHERE FieldName = ? AND FieldType = ?")

    checkIdStmt.setString(1, fieldName)
    checkIdStmt.setString(2, fieldType)

    val checkIdResult = checkIdStmt.executeQuery()

    if(checkIdResult.next()){
      val res = checkIdResult.getInt(1)
      res
    } else {
      val insertIdStmt = connection.prepareStatement(PostgreSqlTables.fieldSignatureTable.buildInsertSql(ignoreConflicts = false),
        Statement.RETURN_GENERATED_KEYS)
      insertIdStmt.setString(1, fieldName)
      insertIdStmt.setString(2, fieldType)
      insertIdStmt.executeUpdate()

      val insertIdResult = insertIdStmt.getGeneratedKeys

      if(insertIdResult.next()){
        val res = insertIdResult.getInt(1)
        res
      } else {
        throw new Exception("Failed to insert field signature")
      }
    }
  }

  private def assertVersionPresentAndGetId(connection: Connection, versionNumber: String): Int = {
    val checkIdStmt =
      connection.prepareStatement("SELECT Id FROM " + PostgreSqlTables.versionNumberTable.tableName + " WHERE VersionNumber = ?")

    checkIdStmt.setString(1, versionNumber)

    val checkIdResult = checkIdStmt.executeQuery()

    if(checkIdResult.next()){
      checkIdResult.getInt(1)
    } else {
      val insertIdStmt = connection.prepareStatement(PostgreSqlTables.versionNumberTable.buildInsertSql(ignoreConflicts = false),
        Statement.RETURN_GENERATED_KEYS)
      insertIdStmt.setString(1, versionNumber)
      insertIdStmt.executeUpdate()

      val insertIdResult = insertIdStmt.getGeneratedKeys

      if(insertIdResult.next()){
        insertIdResult.getInt(1)
      } else {
        throw new Exception("Failed to insert version number")
      }
    }
  }

  private def storeExceptionsToDefaultValue[T <: Int](evo: ValueEvolution[T],
                                                      prepStmt: PreparedStatement,
                                                      entityId: Int,
                                                      versionToIdMap: Map[String, Int]): Unit = {

    val evoDefault = evo.getDefaultValue

    evo
      .valueToReleasesMap
      .filterKeys(value => value != evoDefault)
      .flatMap( t => t._2.map(r => (t._1, r)))
      .foreach { case (flags: Int, release: String) =>

        prepStmt.setInt(1, entityId)
        prepStmt.setInt(2, versionToIdMap(release))
        prepStmt.setInt(3, flags)
        prepStmt.addBatch()

      }

    prepStmt.executeBatch()
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
