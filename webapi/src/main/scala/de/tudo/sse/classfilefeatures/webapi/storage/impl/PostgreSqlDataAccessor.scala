package de.tudo.sse.classfilefeatures.webapi.storage.impl

import de.tudo.sse.classfilefeatures.common.model.{ClassFileRepresentation, FieldDefinitionRepresentation}
import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.{PostgreSqlConnectionConfiguration, PostgreSqlConnectivity}
import de.tudo.sse.classfilefeatures.webapi.model.{LibraryClassActivationInformation, LibraryClassInformation}
import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor
import de.tudo.sse.classfilefeatures.webapi.storage.impl.PostgreSqlStorageModel.{FieldAccessInstructionEntry, FieldDefinitionEntry, InvocationInstructionEntry, MethodDefinitionEntry}
import de.tudo.sse.classfilefeatures.webapi.storage.model.LibraryClassInformationStorageModel

import java.sql.PreparedStatement
import scala.collection.mutable

class PostgreSqlDataAccessor(override protected val connectionConfiguration: PostgreSqlConnectionConfiguration)
  extends PostgreSqlConnectivity with ClassfileDataAccessor {

  private val versionIdCache: mutable.Map[String, Int] = new mutable.HashMap

  override def verifyConnectivity(): Unit = {

    // connection is a lazy val, so any errors while building it will be thrown here!
    if (!connection.isValid(30)) {
      throw new IllegalStateException("PostgreSQL connection not valid.")
    }
  }

  override def shutdown(): Unit = connection.close()

  override def hasLibrary(libraryName: String): Boolean = {
    val stmt = connection.prepareStatement("SELECT LibraryName FROM Libraries WHERE Libraries.LibraryName = ?;")

    stmt.setString(1, libraryName)

    hasAnyResults(stmt)
  }

  override def getLibraryNames(offset: Int, count: Int): Array[String] = {
    //TODO: Pagination
    val stmt = connection.prepareStatement("SELECT LibraryName FROM Libraries;")

    resultsAsStringArray(stmt)
  }

  override def hasRelease(libraryName: String, releaseName: String): Boolean = {
    val stmt = connection.prepareStatement("SELECT Libraries.Id FROM Libraries " +
      "LEFT JOIN Rel_Libraries_VersionNumbers ON Libraries.Id = Rel_Libraries_VersionNumbers.LibraryId " +
      "LEFT JOIN VersionNumbers ON Rel_Libraries_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ? AND VersionNumbers.VersionNumber = ?")

    stmt.setString(1, libraryName)
    stmt.setString(2, releaseName)

    hasAnyResults(stmt)
  }

  override def hasLibraryClass(libraryName: String, className: String): Boolean = {
    val stmt = connection.prepareStatement("SELECT Libraries.Id FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "WHERE Libraries.LibraryName = ? AND Classfiles.ThisType = ?")

    stmt.setString(1, libraryName)
    stmt.setString(2, className)

    hasAnyResults(stmt)
  }

  override def hasReleaseClass(libraryName: String, releaseName: String, className: String): Boolean = {
    val stmt = connection.prepareStatement("SELECT Libraries.Id FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "LEFT JOIN Rel_Classfiles_VersionNumbers ON Classfiles.Id = Rel_Classfiles_VersionNumbers.ClassfileId " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ? AND VersionNumbers.VersionNumber = ? AND Classfiles.ThisType = ?")

    stmt.setString(1, libraryName)
    stmt.setString(2, releaseName)
    stmt.setString(3, className)

    hasAnyResults(stmt)
  }

  override def getLibraryClassNames(libraryName: String): Array[String] = {
    val stmt = connection.prepareStatement("SELECT Classfiles.ThisType, VersionNumbers.VersionNumber FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "LEFT JOIN Rel_Classfiles_VersionNumbers ON Classfiles.Id = Rel_Classfiles_VersionNumbers.ClassfileId " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ?")

    stmt.setString(1, libraryName)

    resultsAsStringArray(stmt)
  }

  override def getLibraryClassActivationInformation(libraryName: String): Array[(String, Set[String])] = {
    val stmt = connection.prepareStatement("SELECT Classfiles.ThisType, VersionNumbers.VersionNumber FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "LEFT JOIN Rel_Classfiles_VersionNumbers ON Classfiles.Id = Rel_Classfiles_VersionNumbers.ClassfileId " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ?")

    stmt.setString(1, libraryName)

    val rs = stmt.executeQuery()

    val classToVersionMap = new mutable.HashMap[String, mutable.HashSet[String]]()

    while(rs.next()){
      val className = rs.getString(1)
      val version = rs.getString(2)

      if(!classToVersionMap.contains(className)){
        classToVersionMap.put(className, mutable.HashSet(version))
      } else {
        classToVersionMap(className).add(version)
      }
    }


    classToVersionMap
      .map( t => (t._1, t._2.toSet))
      .toArray
  }

  override def getLibraryClassInformation(libraryName: String, className: String): LibraryClassInformationStorageModel = {
    val stmt = connection.prepareStatement("SELECT Classfiles.Id, VersionNumbers.VersionNumber, ClassFiles.DefaultFlags, ClassFiles.DefaultSuperType FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "LEFT JOIN Rel_Classfiles_VersionNumbers ON Classfiles.Id = Rel_Classfiles_VersionNumbers.ClassfileId " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ? AND Classfiles.ThisType = ?")

    stmt.setString(1, libraryName)
    stmt.setString(2, className)

    val rs = stmt.executeQuery()

    var classFileId = -1
    val activeIn = new mutable.HashSet[String]()
    var defaultFlags = -1
    var defaultSuperType: Option[String] = None

    while(rs.next()) {
      classFileId = rs.getInt(1)
      activeIn.add(rs.getString(2))
      defaultFlags = rs.getInt(3)

      defaultSuperType = Option(rs.getString(4))
    }

    stmt.close()

    val superTypeStmt = connection.prepareStatement("SELECT Rel_Classfiles_SuperType.SuperType, VersionNumbers.VersionNumber FROM Rel_Classfiles_SuperType " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_SuperType.VersionId = VersionNumbers.Id " +
      "WHERE Rel_Classfiles_SuperType.ClassfileId = ?")

    superTypeStmt.setInt(1, classFileId)
    val superTypeResult = superTypeStmt.executeQuery()
    val superTypeValueToVersionsMap = new mutable.HashMap[Option[String], mutable.Set[String]]()

    while(superTypeResult.next()){
      val superTypeValue = Option(superTypeResult.getString(1))
      val versionString = superTypeResult.getString(2)

      if(!superTypeValueToVersionsMap.contains(superTypeValue)) superTypeValueToVersionsMap.put(superTypeValue, mutable.HashSet(versionString))
      else superTypeValueToVersionsMap(superTypeValue).add(versionString)
    }
    superTypeStmt.close()

    val defaultSuperTypeAppliesTo = activeIn.toSet.diff(superTypeValueToVersionsMap.values.flatten.toSet)
    superTypeValueToVersionsMap.put(defaultSuperType, mutable.Set(defaultSuperTypeAppliesTo.toSeq :_*))

    val flagsStmt = connection.prepareStatement("SELECT Rel_Classfiles_AccessFlags.Flags, VersionNumbers.VersionNumber FROM Rel_Classfiles_AccessFlags " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_AccessFlags.VersionId = VersionNumbers.Id " +
      "WHERE Rel_Classfiles_AccessFlags.ClassfileId = ?")

    flagsStmt.setInt(1, classFileId)
    val flagsResult = flagsStmt.executeQuery()
    val flagsValueToVersionsMap = new mutable.HashMap[Int, mutable.Set[String]]()

    while(flagsResult.next()){
      val flagsValue = flagsResult.getInt(1)
      val versionString = flagsResult.getString(2)

      if(!flagsValueToVersionsMap.contains(flagsValue)) flagsValueToVersionsMap.put(flagsValue, mutable.HashSet(versionString))
      else flagsValueToVersionsMap(flagsValue).add(versionString)
    }
    flagsStmt.close()

    val defaultFlagsApplyTo = activeIn.toSet.diff(flagsValueToVersionsMap.values.flatten.toSet)
    flagsValueToVersionsMap.put(defaultFlags, mutable.Set(defaultFlagsApplyTo.toSeq :_*))

    val methodsStmt = connection.prepareStatement("SELECT Methods.Name, Methods.Descriptor, VersionNumbers.VersionNumber FROM Methods " +
      "LEFT JOIN Rel_Methods_VersionNumbers ON Rel_Methods_VersionNumbers.MethodId = Methods.Id " +
      "LEFT JOIN VersionNumbers ON Rel_Methods_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Methods.ClassfileId = ?")

    methodsStmt.setInt(1, classFileId)
    val methodsResult = methodsStmt.executeQuery()
    val methodValueToVersionsMap = new mutable.HashMap[String, mutable.Set[String]]()

    while(methodsResult.next()){
      val methodName = methodsResult.getString(1) + methodsResult.getString(2)
      val versionString = methodsResult.getString(3)

      if(!methodValueToVersionsMap.contains(methodName)) methodValueToVersionsMap.put(methodName, mutable.HashSet(versionString))
      else methodValueToVersionsMap(methodName).add(versionString)
    }

    methodsStmt.close()

    LibraryClassInformationStorageModel(className,
      activeIn.toArray,
      superTypeValueToVersionsMap.mapValues(_.toArray).toMap,
      flagsValueToVersionsMap.mapValues(_.toArray).toMap,
      methodValueToVersionsMap.mapValues(_.toArray).toMap)
  }

  override def getReleaseClassNames(libraryName: String, releaseName: String): Array[String] = {
    val stmt = connection.prepareStatement("SELECT Classfiles.ThisType FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "LEFT JOIN Rel_Classfiles_VersionNumbers ON Classfiles.Id = Rel_Classfiles_VersionNumbers.ClassfileId " +
      "LEFT JOIN VersionNumbers ON Rel_Classfiles_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ? AND VersionNumbers.VersionNumber = ?")

    stmt.setString(1, libraryName)
    stmt.setString(2, releaseName)

    resultsAsStringArray(stmt)
  }

  override def getReleaseNames(libraryName: String): Array[String] = {
    val stmt = connection.prepareStatement("SELECT VersionNumbers.VersionNumber FROM Libraries " +
      "LEFT JOIN Rel_Libraries_VersionNumbers ON Libraries.Id = Rel_Libraries_VersionNumbers.LibraryId " +
      "LEFT JOIN VersionNumbers ON Rel_Libraries_VersionNumbers.VersionId = VersionNumbers.Id " +
      "WHERE Libraries.LibraryName = ?")

    stmt.setString(1, libraryName)

    resultsAsStringArray(stmt)
  }

  private def hasAnyResults(stmt: PreparedStatement): Boolean = {

    val rs = stmt.executeQuery()

    val present = rs.next()

    rs.close()
    stmt.close()

    present
  }




  override def getClassRepresentation(libraryName: String,
                                      releaseName: String,
                                      className: String): ClassFileRepresentation = {

    // No need to re-check that class is active in release, this is done beforehand!

    val versionId = getIdForVersion(releaseName)

    val stmt = connection.prepareStatement("SELECT Classfiles.Id, Classfiles.ThisType, Classfiles.DefaultFlags, " +
      "Classfiles.DefaultMajorVersion, Classfiles.DefaultMinorVersion, Classfiles.DefaultSuperType FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "WHERE Libraries.LibraryName = ? AND Classfiles.ThisType = ?")

    stmt.setString(1, libraryName)
    stmt.setString(2, className)


    val rs = stmt.executeQuery()

    if(rs.next()){
      val cfId = rs.getInt(1)
      val thisType = rs.getString(2)
      val defaultFlags = rs.getInt(3)
      val defaultMajor = rs.getInt(4)
      val defaultMinor = rs.getInt(5)
      val defaultSuperType = Option(rs.getString(6))

      stmt.close()

      val entry = new PostgreSqlStorageModel.ClassFileEntry(cfId, thisType, defaultFlags, defaultMajor, defaultMinor, defaultSuperType)

      val flagsStmt = connection.prepareStatement("SELECT Rel_Classfiles_AccessFlags.Flags FROM Rel_Classfiles_AccessFlags " +
        "WHERE Rel_Classfiles_AccessFlags.ClassfileId = ? AND Rel_Classfiles_AccessFlags.VersionId = ?")

      val majorStmt = connection.prepareStatement("SELECT Rel_Classfiles_MajorVersion.MajorVersion FROM Rel_Classfiles_MajorVersion " +
        "WHERE Rel_Classfiles_MajorVersion.ClassfileId = ? AND Rel_Classfiles_MajorVersion.VersionId = ?")

      val minorStmt = connection.prepareStatement("SELECT Rel_Classfiles_MinorVersion.MinorVersion FROM Rel_Classfiles_MinorVersion " +
        "WHERE Rel_Classfiles_MinorVersion.ClassfileId = ? AND Rel_Classfiles_MinorVersion.VersionId = ?")

      val superTypeStmt = connection.prepareStatement("SELECT Rel_Classfiles_SuperType.SuperType FROM Rel_Classfiles_SuperType " +
        "WHERE Rel_Classfiles_SuperType.ClassfileId = ? AND Rel_Classfiles_SuperType.VersionId = ?")

      val interfaceStmt = connection.prepareStatement("SELECT Rel_Classfiles_Interface.Interface FROM Rel_Classfiles_Interface " +
        "WHERE Rel_Classfiles_Interface.ClassfileId = ? AND Rel_Classfiles_Interface.VersionId = ?")

      val allStmts = List(flagsStmt, majorStmt, minorStmt, superTypeStmt, interfaceStmt)

      allStmts.foreach{ current =>
        current.setInt(1, entry.dbId)
        current.setInt(2, versionId)
      }

      val flagsResult = resultAsIntOptional(flagsStmt)
      val majorResult = resultAsIntOptional(majorStmt)
      val minorResult = resultAsIntOptional(minorStmt)
      val superResult = resultsAsStringArray(superTypeStmt)
      val interfaceResult = resultsAsStringArray(interfaceStmt)

      allStmts.foreach(_.close())

      if(superResult.length > 1) throw new Exception(s"Inconsistent DB: More than one supertype for class $className in library $libraryName")

      if(flagsResult.isDefined) entry.flags = flagsResult.get
      if(majorResult.isDefined) entry.majorVersion = majorResult.get
      if(minorResult.isDefined) entry.minorVersion = minorResult.get
      if(superResult.nonEmpty) entry.superTypeOpt = Option(superResult.head)
      if(interfaceResult.nonEmpty) entry.interfaceTypes = interfaceResult

      val fieldDefStmt = connection.prepareStatement("SELECT FieldDefinitions.Id, FieldDefinitions.DefaultFlags, FieldSignatures.FieldName, FieldSignatures.FieldType FROM FieldDefinitions " +
        "LEFT JOIN Rel_FieldDefinitions_VersionNumbers ON FieldDefinitions.Id = Rel_FieldDefinitions_VersionNumbers.FieldDefinitionId " +
        "LEFT JOIN FieldSignatures ON FieldDefinitions.FieldSignatureId = FieldSignatures.Id " +
        "WHERE FieldDefinitions.ClassfileId = ? AND Rel_FieldDefinitions_VersionNumbers.VersionId = ?")

      fieldDefStmt.setInt(1, entry.dbId)
      fieldDefStmt.setInt(2, versionId)

      val fieldDefs = new mutable.HashSet[FieldDefinitionEntry]

      val fieldDefRs = fieldDefStmt.executeQuery()

      while(fieldDefRs.next()){
        val currentEntry = new FieldDefinitionEntry(fieldDefRs.getInt(1),
          fieldDefRs.getString(3),
          fieldDefRs.getString(4),
          fieldDefRs.getInt(2))

        val fieldDefFlagsStmt = connection.prepareStatement("SELECT Rel_FieldDefinitions_AccessFlags.Flags FROM Rel_FieldDefinitions_AccessFlags " +
          "WHERE Rel_FieldDefinitions_AccessFlags.FieldDefinitionId = ? AND Rel_FieldDefinitions_AccessFlags.VersionId = ?")
        fieldDefFlagsStmt.setInt(1, currentEntry.dbId)
        fieldDefFlagsStmt.setInt(2, versionId)

        val exceptionalFlag = resultAsIntOptional(fieldDefFlagsStmt)

        if(exceptionalFlag.isDefined) currentEntry.flags = exceptionalFlag.get

        fieldDefs.add(currentEntry)
      }

      fieldDefStmt.close()

      // -------------------------
      // |   ALL INVOCATIONS     |
      // -------------------------

      val allInvocationsStmt = connection.prepareStatement("SELECT Methods.Id, InvocationInstructions.TargetMethodName, " +
        "InvocationInstructions.TargetMethodDescriptor, InvocationInstructions.TargetMethodClass, InvocationInstructions.IsInterfaceInvocation, " +
        "InvocationInstructions.InvocationType FROM Methods " +
        "JOIN InvocationInstructions ON Methods.Id = InvocationInstructions.MethodId " +
        "JOIN Rel_InvocationInstructions_VersionNumbers ON InvocationInstructions.Id = Rel_InvocationInstructions_VersionNumbers.InvocationInstructionId " +
        "WHERE Methods.ClassfileId = ? AND Rel_InvocationInstructions_VersionNumbers.VersionId = ?")

      allInvocationsStmt.setInt(1, entry.dbId)
      allInvocationsStmt.setInt(2, versionId)

      val allInvocationsRs = allInvocationsStmt.executeQuery()
      val allInvocationDefs = new mutable.HashSet[InvocationInstructionEntry]

      while(allInvocationsRs.next()){
        allInvocationDefs.add(new InvocationInstructionEntry(allInvocationsRs.getInt(1),
          allInvocationsRs.getString(2),
          allInvocationsRs.getString(3),
          allInvocationsRs.getString(4),
          allInvocationsRs.getBoolean(5),
          allInvocationsRs.getString(6)
        ))
      }

      allInvocationsStmt.close()

      // -------------------------
      // |   ALL FIELD ACCESSES  |
      // -------------------------

      val allFieldAccStmt = connection.prepareStatement("SELECT Methods.Id, FieldSignatures.FieldName, " +
        "FieldSignatures.FieldType, FieldAccessInstructions.FieldClass, FieldAccessInstructions.AccessType FROM Methods " +
        "JOIN FieldAccessInstructions ON Methods.Id = FieldAccessInstructions.MethodId " +
        "JOIN FieldSignatures ON FieldAccessInstructions.FieldSignatureId = FieldSignatures.Id " +
        "JOIN Rel_FieldAccesses_VersionNumbers ON FieldAccessInstructions.Id = Rel_FieldAccesses_VersionNumbers.FieldAccessInstructionId " +
        "WHERE Methods.ClassfileId = ? AND Rel_FieldAccesses_VersionNumbers.VersionId = ?")

      allFieldAccStmt.setInt(1, entry.dbId)
      allFieldAccStmt.setInt(2, versionId)

      val allFieldAccRs = allFieldAccStmt.executeQuery()
      val allFieldAccDefs = new mutable.HashSet[FieldAccessInstructionEntry]

      while(allFieldAccRs.next()){
        allFieldAccDefs.add(new FieldAccessInstructionEntry(allFieldAccRs.getInt(1),
          allFieldAccRs.getString(2),
          allFieldAccRs.getString(3),
          allFieldAccRs.getString(4),
          allFieldAccRs.getString(5)
        ))
      }

      allFieldAccStmt.close()

      // -------------------------
      // |     METHODS           |
      // -------------------------

      val methodStmt = connection.prepareStatement("SELECT Methods.Id, Methods.Name, Methods.Descriptor, Methods.HasBody, " +
        "Rel_Methods_AccessFlags.Flags, Methods.DefaultFlags, " +
        "Rel_Method_MaxStack.MaxStack, Methods.DefaultMaxStack, " +
        "Rel_Method_MaxLocals.MaxLocals, Methods.DefaultMaxLocals FROM Methods " +
        "LEFT JOIN Rel_Methods_VersionNumbers ON Methods.Id = Rel_Methods_VersionNumbers.MethodId " +
        "LEFT JOIN Rel_Methods_AccessFlags ON Methods.Id = Rel_Methods_AccessFlags.MethodId " +
        "LEFT JOIN Rel_Method_MaxStack ON Methods.Id = Rel_Method_MaxStack.MethodId " +
        "LEFT JOIN Rel_Method_MaxLocals ON Methods.Id = Rel_Method_MaxLocals.MethodId " +
        "WHERE Methods.ClassfileId = ? AND Rel_Methods_VersionNumbers.VersionId = ? " +
        "AND (Rel_Methods_AccessFlags.VersionId IS NULL OR Rel_Methods_AccessFlags.VersionId = ?) " +
        "AND (Rel_Method_MaxStack.VersionId IS NULL OR Rel_Method_MaxStack.VersionId = ?) " +
        "AND (Rel_Method_MaxLocals.VersionId IS NULL OR Rel_Method_MaxLocals.VersionId = ?)")
      methodStmt.setInt(1, entry.dbId)
      methodStmt.setInt(2, versionId)
      methodStmt.setInt(3, versionId)
      methodStmt.setInt(4, versionId)
      methodStmt.setInt(5, versionId)

      val methodRs = methodStmt.executeQuery()

      val methodDefs = new mutable.HashSet[MethodDefinitionEntry]

      while(methodRs.next()){

        var flags = methodRs.getInt(5)
        if(methodRs.wasNull()) flags = methodRs.getInt(6)

        // MaxStack -> exceptional value. If not defined -> default value. If not defined -> None
        var maxStack: Option[Int] = Some(methodRs.getInt(7))
        if(methodRs.wasNull()){
          maxStack = Some(methodRs.getInt(8))
          if(methodRs.wasNull()) maxStack = None
        }

        // MaxLocals -> exceptional value. If not defined -> default value. If not defined -> None
        var maxLocals: Option[Int] = Some(methodRs.getInt(9))
        if(methodRs.wasNull()){
          maxLocals = Some(methodRs.getInt(10))
          if(methodRs.wasNull()) maxLocals = None
        }

        val currentEntry = new MethodDefinitionEntry(methodRs.getInt(1),
          methodRs.getString(2),
          methodRs.getString(3),
          methodRs.getBoolean(4),
          flags,
          maxStack,
          maxLocals)

        currentEntry.invocationInstructions = allInvocationDefs
          .filter(entry => entry.methodDbId == currentEntry.dbId)
          .toArray

        currentEntry.fieldAccessInstructions = allFieldAccDefs.
          filter(entry => entry.methodDbId == currentEntry.dbId)
          .toArray

        methodDefs.add(currentEntry)
      }

      methodStmt.close()

      ClassFileRepresentation(entry.flags,
        entry.majorVersion,
        entry.minorVersion,
        entry.thisType,
        entry.superTypeOpt,
        entry.interfaceTypes,
        methodDefs.map(_.toModel).toSeq,
        fieldDefs.map(e => FieldDefinitionRepresentation(e.flags, e.fieldName, e.fieldType)).toSeq)

    } else {
      stmt.close()
      throw new Exception(s"Inconsistent DB: Classfile $className in Library $libraryName not found")
    }

  }

  private def resultAsIntOptional(stmt: PreparedStatement): Option[Int] = {
    val rs = stmt.executeQuery()

    if(!rs.next()){
      rs.close()
      stmt.close()

      None
    } else {
      val result = Option(rs.getInt(1))

      if(rs.next()) throw new Exception(s"Inconsistent DB: More than one int value for query")

      rs.close()
      stmt.close()

      result
    }
  }

  private def resultsAsStringArray(stmt: PreparedStatement): Array[String] = {

    val rs = stmt.executeQuery()

    val stringValues = new mutable.HashSet[String]

    while(rs.next()) { stringValues.add(rs.getString(1)) }

    rs.close()
    stmt.close()

    stringValues.toArray
  }

  private def getIdForVersion(versionName: String): Int = {

    if(versionIdCache.contains(versionName)){
      versionIdCache(versionName)
    } else {
      val stmt = connection.prepareStatement("SELECT VersionNumbers.Id FROM VersionNumbers WHERE VersionNumbers.VersionNumber = ?")
      stmt.setString(1, versionName)

      val rs = stmt.executeQuery()

      var id = -1

      while(rs.next()){
        if(id == -1) id = rs.getInt(1)
        else throw new Exception(s"Multiple IDs for version $versionName")
      }

      stmt.close()

      if(id == -1) throw new Exception(s"Version not found in DB: $versionName")

      versionIdCache.put(versionName, id)

      id
    }

  }

}
