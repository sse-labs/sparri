package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql
import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.{PostgreSqlConnectionConfiguration, PostgreSqlConnectivity, PostgreSqlTables}
import de.tudo.sse.classfilefeatures.extractor.model.{LibraryClassFileModel, LibraryClassfileFeatureModel, ValueEvolution}
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler

import java.sql.{Connection, PreparedStatement, Statement}
import java.util.concurrent.locks.ReentrantLock
import scala.util.{Success, Try}

class PostgreSqlStorageHandler(config: PostgreSqlConnectionConfiguration) extends ClassfileFeatureStorageHandler with PostgreSqlConnectivity {

  override protected val connectionConfiguration: postgresql.PostgreSqlConnectionConfiguration = config

  private val libraryIndexRWLock: ReentrantLock = new ReentrantLock()

  override def verifyConnectivity(): Unit = {

    // connection is a lazy val, so any errors while building it will be thrown here!
    if (!connection.isValid(30)) {
      throw new IllegalStateException("PostgreSQL connection not valid.")
    }
  }

  override def initialize(): Unit = {
    connection.setAutoCommit(false)

    val stmt = connection.createStatement()

    PostgreSqlTables.allEntityTables.foreach { table =>
      stmt.addBatch(table.tableDefinitionSql)
    }

    //TODO: Execute this only once!
    //stmt.addBatch("CREATE INDEX idx_field_signature ON " + PostgreSqlTables.fieldSignatureTable.tableName + " (FieldName, FieldType)")

    PostgreSqlTables.allRelationTables.foreach { table =>
      stmt.addBatch(table.tableDefinitionSql)
    }

    if (stmt.executeBatch().contains(Statement.EXECUTE_FAILED)) {
      stmt.close()
      throw new Exception("Failed to create tables in database")
    }

    stmt.close()
    connection.commit()
  }

  override def isLibraryPresent(libraryIdentifier: String): Boolean = {
    libraryIndexRWLock.lock()

    var libraryPresent = false

    try {
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
    //TODO: Tackle performance issues

    // Threadsafe way to check whether library is present. Only proceed if it was not present before.
    libraryIndexRWLock.lock()

    var libraryWasPresent = false
    var libraryId = -1

    try {
      libraryWasPresent = isLibraryPresent(model.libraryIdentifier)

      if (!libraryWasPresent) {

        // Insert library into index table before releasing the lock
        val prepStmt = connection.prepareStatement(PostgreSqlTables.libraryTable.buildInsertSql(ignoreConflicts = false),
          Statement.RETURN_GENERATED_KEYS)

        prepStmt.setString(1, model.libraryIdentifier)
        prepStmt.executeUpdate()

        val result = prepStmt.getGeneratedKeys

        if (result.next()) {
          libraryId = result.getInt(1)
        }

        result.close()
        prepStmt.close()
      }
    } finally {
      libraryIndexRWLock.unlock()
    }


    if (libraryWasPresent) {
      // If the library was present before, we don't have to do anything
      Success()
    } else {
      Try {
        //If it was not present before, we have to store all data

        // Adds all versions to table and stores id mapping
        val versionToIdMap = model.releases.map(r => (r, assertVersionPresentAndGetId(connection, r))).toMap
        connection.commit()

        // Adds all flags to table. No need to store id mapping, flag value is PK
        val allFlagsInModel = model.allClassfileModels.flatMap(_.flagsEvolution.valueToReleasesMap.keySet) ++
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

        log.info(s"[${model.libraryIdentifier}] Start building field index...")
        // Assert that all field signature are present and build an id lookup
        val allFieldSignaturesInModel: Set[(String, String)] =
          (model.allClassfileModels.flatMap(_.fieldDefinitionEvolutions).map(lfdm => (lfdm.fieldName, lfdm.fieldTypeJvmName)) ++
            model.allClassfileModels.flatMap(_.methodEvolutions).flatMap(_.fieldAccessEvolutions).map(lfaim => (lfaim.fieldName, lfaim.fieldTypeJvmName))).toSet

        val fieldSignatureToIdMap = allFieldSignaturesInModel.map(sig => (sig._1 + sig._2, assertFieldSignaturePresentAndGetId(connection, sig._1, sig._2))).toMap
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
        val classFileToMajorVersionInsertStmt = connection.prepareStatement(PostgreSqlTables.classFileMajorVersionValueTable.buildInsertSql(ignoreConflicts = false))
        val classFileToMinorVersionInsertStmt = connection.prepareStatement(PostgreSqlTables.classFileMinorVersionValueTable.buildInsertSql(ignoreConflicts = false))
        val classFileToSuperTypeInsertStmt = connection.prepareStatement(PostgreSqlTables.classFileSuperTypeValueTable.buildInsertSql(ignoreConflicts = false))
        val classFileToInterfaceInsertStmt = connection.prepareStatement(PostgreSqlTables.classFileInterfaceValueTable.buildInsertSql(ignoreConflicts = false))

        val fieldDefinitionInsertStmt = connection.prepareStatement(PostgreSqlTables.fieldDefinitionTable.buildInsertSql(ignoreConflicts = false),
          Statement.RETURN_GENERATED_KEYS)

        val fieldDefToVersionInsertStmt = connection.prepareStatement(PostgreSqlTables.fieldDefinitionsToVersionsTable.buildInsertSql(ignoreConflicts = false))
        val fieldDefToFlagsInsertStmt = connection.prepareStatement(PostgreSqlTables.fieldDefinitionFlagValueTable.buildInsertSql(ignoreConflicts = false))


        model.allClassfileModels.foreach { lcfm =>

          val cfDefaultFlags = lcfm.flagsEvolution.getDefaultValue
          val cfDefaultMajor = lcfm.majorVersionEvolution.getDefaultValue
          val cfDefaultMinor = lcfm.minorVersionEvolution.getDefaultValue
          val cfDefaultSuperType = lcfm.superTypeEvolution.getDefaultValue

          // Store classfile itself
          classFileInsertStmt.setInt(1, libraryId)
          classFileInsertStmt.setString(2, lcfm.classFileThisTypeFqn)
          classFileInsertStmt.setInt(3, cfDefaultFlags)
          classFileInsertStmt.setInt(4, cfDefaultMajor)
          classFileInsertStmt.setInt(5, cfDefaultMinor)

          if (cfDefaultSuperType.isDefined) classFileInsertStmt.setString(6, cfDefaultSuperType.get)
          else classFileInsertStmt.setNull(6, java.sql.Types.VARCHAR)

          classFileInsertStmt.executeUpdate()

          val rs = classFileInsertStmt.getGeneratedKeys

          val classFileId = if (rs.next()) rs.getInt(1)
          else throw new Exception(s"Failed to store classfile ${lcfm.classFileThisTypeFqn}")
          connection.commit()

          // Store all methods of classfile
          log.info(s"[${model.libraryIdentifier}] Begin store methods for ${lcfm.classFileThisTypeFqn}...")
          storeAllMethods(lcfm, classFileId, versionToIdMap, fieldSignatureToIdMap)
          log.info(s"[${model.libraryIdentifier}] End store methods for ${lcfm.classFileThisTypeFqn}.")

          // Store exceptions from default values for classfile
          storeExceptionsToDefaultValue(lcfm.flagsEvolution, classFileToFlagsInsertStmt, classFileId, versionToIdMap)
          storeExceptionsToDefaultValue(lcfm.majorVersionEvolution, classFileToMajorVersionInsertStmt, classFileId, versionToIdMap)
          storeExceptionsToDefaultValue(lcfm.minorVersionEvolution, classFileToMinorVersionInsertStmt, classFileId, versionToIdMap)
          storeExceptionsToDefaultValue(lcfm.superTypeEvolution, classFileToSuperTypeInsertStmt, classFileId, versionToIdMap)

          lcfm.interfacesEvolution.getReleaseToValueTuples.foreach { case (v: String, interface: String) =>
            classFileToInterfaceInsertStmt.setInt(1, classFileId)
            classFileToInterfaceInsertStmt.setInt(2, versionToIdMap(v))
            classFileToInterfaceInsertStmt.setString(3, interface)
            classFileToInterfaceInsertStmt.addBatch()
          }

          classFileToInterfaceInsertStmt.executeBatch()

          // Store classfile field definitions
          lcfm.fieldDefinitionEvolutions.foreach { lfdm =>

            val fdDefaultFlags = lfdm.flagsEvolution.getDefaultValue

            // Store field definition itself
            fieldDefinitionInsertStmt.setInt(1, classFileId)
            fieldDefinitionInsertStmt.setInt(2, fieldSignatureToIdMap(lfdm.fieldName + lfdm.fieldTypeJvmName))
            fieldDefinitionInsertStmt.setInt(3, fdDefaultFlags)

            fieldDefinitionInsertStmt.executeUpdate()

            val fdrs = fieldDefinitionInsertStmt.getGeneratedKeys

            val fieldDefinitionId = if (fdrs.next()) fdrs.getInt(1)
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
        classFileToMajorVersionInsertStmt.close()
        classFileToMinorVersionInsertStmt.close()
        fieldDefinitionInsertStmt.close()
        fieldDefToVersionInsertStmt.close()
        fieldDefToFlagsInsertStmt.close()
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

    if (checkIdResult.next()) {
      val res = checkIdResult.getInt(1)
      res
    } else {
      val insertIdStmt = connection.prepareStatement(PostgreSqlTables.fieldSignatureTable.buildInsertSql(ignoreConflicts = false),
        Statement.RETURN_GENERATED_KEYS)
      insertIdStmt.setString(1, fieldName)
      insertIdStmt.setString(2, fieldType)
      insertIdStmt.executeUpdate()

      val insertIdResult = insertIdStmt.getGeneratedKeys

      if (insertIdResult.next()) {
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

    if (checkIdResult.next()) {
      checkIdResult.getInt(1)
    } else {
      val insertIdStmt = connection.prepareStatement(PostgreSqlTables.versionNumberTable.buildInsertSql(ignoreConflicts = false),
        Statement.RETURN_GENERATED_KEYS)
      insertIdStmt.setString(1, versionNumber)
      insertIdStmt.executeUpdate()

      val insertIdResult = insertIdStmt.getGeneratedKeys

      if (insertIdResult.next()) {
        insertIdResult.getInt(1)
      } else {
        throw new Exception("Failed to insert version number")
      }
    }
  }

  private def storeExceptionsToDefaultValue[T](evo: ValueEvolution[T],
                                               prepStmt: PreparedStatement,
                                               entityId: Int,
                                               versionIdLookup: String => Int): Unit = {

    val evoDefault = evo.getDefaultValue

    evo
      .valueToReleasesMap
      .filterKeys(value => value != evoDefault)
      .flatMap(t => t._2.map(r => (t._1, r)))
      .foreach {
        case (flags: Int, release: String) =>

          prepStmt.setInt(1, entityId)
          prepStmt.setInt(2, versionIdLookup(release))
          prepStmt.setInt(3, flags)
          prepStmt.addBatch()

        case (superType: Option[String], release: String) =>

          prepStmt.setInt(1, entityId)
          prepStmt.setInt(2, versionIdLookup(release))

          if (superType.isDefined) prepStmt.setString(3, superType.get)
          else prepStmt.setNull(3, java.sql.Types.VARCHAR)

          prepStmt.addBatch()
      }

    prepStmt.executeBatch()
  }

  private def storeAllMethods(lcfm: LibraryClassFileModel,
                              parentClassfileId: Int,
                              versionLookup: String => Int,
                              fieldSignatureLookup: String => Int): Unit = {

    val methodInsertStmt = connection.prepareStatement(PostgreSqlTables.methodTable.buildInsertSql(ignoreConflicts = false),
      Statement.RETURN_GENERATED_KEYS)

    val methodToVersionInsertStmt =
      connection.prepareStatement(PostgreSqlTables.methodsToVersionsTable.buildInsertSql(ignoreConflicts = false))

    val methodToFlagsInsertStmt =
      connection.prepareStatement(PostgreSqlTables.methodFlagValueTable.buildInsertSql(ignoreConflicts = false))

    val methodToMaxStackInsertStmt =
      connection.prepareStatement(PostgreSqlTables.methodMaxStackValueTable.buildInsertSql(ignoreConflicts = false))

    val methodToMaxLocalsInsertStmt =
      connection.prepareStatement(PostgreSqlTables.methodMaxLocalsValueTable.buildInsertSql(ignoreConflicts = false))

    val invocationInstructionInsertStmt =
      connection.prepareStatement(PostgreSqlTables.invocationInstructionsTable.buildInsertSql(ignoreConflicts = false),
        Statement.RETURN_GENERATED_KEYS)

    val invocationInstrToVersionInsertStmt =
      connection.prepareStatement(PostgreSqlTables.invocationInstructionsToVersionsTable.buildInsertSql(ignoreConflicts = false))

    val fieldAccessInstructionInsertStmt =
      connection.prepareStatement(PostgreSqlTables.fieldAccessInstructionsTable.buildInsertSql(ignoreConflicts = false),
        Statement.RETURN_GENERATED_KEYS)

    val fieldAccToVersionInsertStmt =
      connection.prepareStatement(PostgreSqlTables.fieldAccessesToVersionsTable.buildInsertSql(ignoreConflicts = false))

    val methodIdLookup = lcfm.methodEvolutions.map { lmm =>
      val methodDefaultFlags = lmm.flagsEvolution.getDefaultValue
      // For MaxStack and MaxLocals there may not be any value at all (in case of abstract methods)
      val methodDefaultMaxStack = lmm.maxStackEvolution.getDefaultValueOpt
      val methodDefaultMaxLocals = lmm.maxLocalsEvolution.getDefaultValueOpt

      methodInsertStmt.setInt(1, parentClassfileId)
      methodInsertStmt.setString(2, lmm.methodName)
      methodInsertStmt.setString(3, lmm.jvmMethodDescriptor)
      methodInsertStmt.setInt(4, methodDefaultFlags)

      if (methodDefaultMaxStack.isDefined) methodInsertStmt.setInt(5, methodDefaultMaxStack.get)
      else methodInsertStmt.setNull(5, java.sql.Types.INTEGER)

      if (methodDefaultMaxLocals.isDefined) methodInsertStmt.setInt(6, methodDefaultMaxLocals.get)
      else methodInsertStmt.setNull(6, java.sql.Types.INTEGER)

      methodInsertStmt.setBoolean(7, lmm.hasBody)

      methodInsertStmt.executeUpdate()
      val mrs = methodInsertStmt.getGeneratedKeys

      val methodId = if (mrs.next()) mrs.getInt(1)
      else throw new Exception(s"Failed to store methods for classfile ${lcfm.classFileThisTypeFqn}")

      (lmm.identifier, methodId)
    }.toMap
    methodInsertStmt.close()
    connection.commit()

    lcfm.methodEvolutions.foreach { lmm =>

      val methodId = methodIdLookup(lmm.identifier)

      // Store exceptions to default flag for method
      storeExceptionsToDefaultValue(lmm.flagsEvolution, methodToFlagsInsertStmt, methodId, versionLookup)

      if (lmm.hasBody) {
        storeExceptionsToDefaultValue(lmm.maxStackEvolution, methodToMaxStackInsertStmt, methodId, versionLookup)
        storeExceptionsToDefaultValue(lmm.maxLocalsEvolution, methodToMaxLocalsInsertStmt, methodId, versionLookup)
      }

      lmm.allActiveReleases.foreach { v =>

        methodToVersionInsertStmt.setInt(1, methodId)
        methodToVersionInsertStmt.setInt(2, versionLookup(v))
        methodToVersionInsertStmt.addBatch()
      }

      methodToVersionInsertStmt.executeBatch()

      val invokeIdLookup = lmm.invocationEvolutions.map { liim =>
        invocationInstructionInsertStmt.setInt(1, methodId)
        invocationInstructionInsertStmt.setString(2, liim.targetMethodName)
        invocationInstructionInsertStmt.setString(3, liim.targetMethodJvmDescriptor)
        invocationInstructionInsertStmt.setString(4, liim.targetMethodDeclaredClassFqn)
        invocationInstructionInsertStmt.setBoolean(5, liim.isInterfaceInvocation)
        invocationInstructionInsertStmt.setString(6, liim.invocationType.toString)

        invocationInstructionInsertStmt.executeUpdate()
        val irs = invocationInstructionInsertStmt.getGeneratedKeys

        val instructionId = if (irs.next()) irs.getInt(1)
        else throw new Exception(s"Failed to insert instruction $liim")

        (liim.identifier, instructionId)
      }.toMap

      val fieldAccessIdLookup = lmm.fieldAccessEvolutions.map { lfaim =>
        fieldAccessInstructionInsertStmt.setInt(1, methodId)
        invocationInstructionInsertStmt.setInt(2, fieldSignatureLookup(lfaim.fieldName + lfaim.fieldTypeJvmName))
        invocationInstructionInsertStmt.setString(3, lfaim.fieldDeclaredClassFqn)
        invocationInstructionInsertStmt.setString(4, lfaim.fieldAccessType.toString)

        invocationInstructionInsertStmt.executeUpdate()
        val irs = invocationInstructionInsertStmt.getGeneratedKeys

        val instructionId = if (irs.next()) irs.getInt(1)
        else throw new Exception(s"Failed to insert instruction $lfaim")

        (lfaim.identifier, instructionId)
      }.toMap

      connection.commit()


      lmm.invocationEvolutions.foreach { liim =>
        liim.allActiveReleases.foreach { v =>
          invocationInstrToVersionInsertStmt.setInt(1, invokeIdLookup(liim.identifier))
          invocationInstrToVersionInsertStmt.setInt(2, versionLookup(v))
          invocationInstrToVersionInsertStmt.addBatch()
        }
        invocationInstrToVersionInsertStmt.executeBatch()
      }

      lmm.fieldAccessEvolutions.foreach { lfaim =>
        lfaim.allActiveReleases.foreach { v =>
          fieldAccToVersionInsertStmt.setInt(1, fieldAccessIdLookup(lfaim.identifier))
          fieldAccToVersionInsertStmt.setInt(2, versionLookup(v))
          fieldAccToVersionInsertStmt.addBatch()
        }

        fieldAccToVersionInsertStmt.executeBatch()
      }

    }

    methodToFlagsInsertStmt.close()
    methodToMaxStackInsertStmt.close()
    methodToMaxLocalsInsertStmt.close()
    methodToVersionInsertStmt.close()
    invocationInstructionInsertStmt.close()
    invocationInstrToVersionInsertStmt.close()
    fieldAccessInstructionInsertStmt.close()
    fieldAccToVersionInsertStmt.close()
  }
}