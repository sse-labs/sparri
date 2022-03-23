package de.tudo.sse.classfilefeatures.common.storage.impl.postgresql

import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.PostgreSqlDataTypes.PostgreSqlDataType

import java.sql.{Connection, PreparedStatement}

object PostgreSqlTables {

  def allEntityTables: List[PostgreSqlTable] = List(libraryTable, versionNumberTable, accessFlagsTable, fieldSignatureTable,
    classFileTable, fieldDefinitionTable, methodTable, invocationInstructionsTable, fieldAccessInstructionsTable)

  val libraryTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Libraries", autoIdColumn = true,
    columns = List(PostgreSqlTableColumn("LibraryName", PostgreSqlDataTypes.Text)))

  val versionNumberTable: PostgreSqlTable = new PostgreSqlTable(tableName = "VersionNumbers", autoIdColumn = true,
    columns = List(PostgreSqlTableColumn("VersionNumber", PostgreSqlDataTypes.ShortText)))

  val accessFlagsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "AccessFlags",
    columns = List(PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int, isPrimaryKey = true)))

  val fieldSignatureTable: PostgreSqlTable = new PostgreSqlTable(tableName = "FieldSignatures", autoIdColumn = true,
    columns = List(
      PostgreSqlTableColumn("FieldName", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("FieldType", PostgreSqlDataTypes.Text)
    ))

  //TODO: Supertype, Interfaces, MajorVersion, MinorVersion
  val classFileTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Classfiles", autoIdColumn = true,
    columns = List(
      PostgreSqlTableColumn("LibraryId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("ThisType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("DefaultFlags", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("LibraryId"), libraryTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("DefaultFlags"), accessFlagsTable.tableName, List("Flags"))
    )
  )

  val methodTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Methods", autoIdColumn = true,
    columns = List(
      PostgreSqlTableColumn("ClassfileId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("Name", PostgreSqlDataTypes.ShortText),
      PostgreSqlTableColumn("Descriptor", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("DefaultFlags", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("DefaultMaxStack", PostgreSqlDataTypes.Int, isNullable = true),
      PostgreSqlTableColumn("DefaultMaxLocals", PostgreSqlDataTypes.Int, isNullable = true),
      PostgreSqlTableColumn("HasBody", PostgreSqlDataTypes.Bool)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("ClassfileId"), classFileTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("DefaultFlags"), accessFlagsTable.tableName, List("Flags"))
    ))

  val fieldDefinitionTable: PostgreSqlTable = new PostgreSqlTable(tableName = "FieldDefinitions", autoIdColumn = true,
    columns = List(
      PostgreSqlTableColumn("ClassfileId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("FieldSignatureId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("DefaultFlags", PostgreSqlDataTypes.Int),
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("DefaultFlags"), accessFlagsTable.tableName, List("Flags")),
      PostgreSqlForeignKeyConstraint(List("ClassfileId"), classFileTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("FieldSignatureId"), fieldSignatureTable.tableName, List("Id"))
    )
  )

  // IMPROVE: Move Strings to table
  // IMPROVE: Separate actual invocation definitions and method-to-instruction relation
  val invocationInstructionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "InvocationInstructions", autoIdColumn = true,
    columns = List(
      PostgreSqlTableColumn("MethodId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("TargetMethodName", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("TargetMethodDescriptor", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("TargetMethodClass", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("IsInterfaceInvocation", PostgreSqlDataTypes.Bool),
      PostgreSqlTableColumn("InvocationType", PostgreSqlDataTypes.ShortText)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("MethodId"), methodTable.tableName, List("Id"))
    )
  )

  // IMPROVE: Move Strings to table
  // IMPROVE: Separate actual invocation definitions and method-to-instruction relation
  val fieldAccessInstructionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "FieldAccessInstructions", autoIdColumn = true,
    columns = List(
      PostgreSqlTableColumn("MethodId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("FieldSignatureId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("FieldClass", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("AccessType", PostgreSqlDataTypes.ShortText)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("MethodId"), methodTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("FieldSignatureId"), fieldSignatureTable.tableName, List("Id"))
    )
  )


  /*  -------------------------------------------------------------
      |                START OF RELATION TABLES                   |
      -------------------------------------------------------------*/

  def allRelationTables: List[PostgreSqlTable] = List(librariesToVersionsTable, classFilesToVersionsTable, fieldDefinitionsToVersionsTable,
    classFileFlagValueTable, fieldDefinitionFlagValueTable, methodsToVersionsTable, methodFlagValueTable, methodMaxStackValueTable, methodMaxLocalsValueTable,
    fieldAccessesToVersionsTable, invocationInstructionsToVersionsTable)

  // Entity-to-Active-Version Relations first
  val librariesToVersionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Libraries_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("LibraryId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("LibraryId"), libraryTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val classFilesToVersionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Classfiles_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("ClassfileId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("ClassfileId"), classFileTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val methodsToVersionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Methods_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("MethodId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("MethodId"), methodTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val fieldDefinitionsToVersionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_FieldDefinitions_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("FieldDefinitionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("FieldDefinitionId"), fieldDefinitionTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val invocationInstructionsToVersionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_InvocationInstructions_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("InvocationInstructionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("InvocationInstructionId"), invocationInstructionsTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val fieldAccessesToVersionsTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_FieldAccesses_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("FieldAccessInstructionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("FieldAccessInstructionId"), fieldAccessInstructionsTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )



  // Default Value Exception tables: Flags

  val classFileFlagValueTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Classfiles_AccessFlags",
    columns = List(
      PostgreSqlTableColumn("ClassfileId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("ClassfileId"), classFileTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("Flags"), accessFlagsTable.tableName, List("Flags"))
    )
  )

  val methodFlagValueTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Methods_AccessFlags",
    columns = List(
      PostgreSqlTableColumn("MethodId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("MethodId"), methodTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("Flags"), accessFlagsTable.tableName, List("Flags"))
    )
  )

  val methodMaxStackValueTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Method_MaxStack",
    columns = List(
      PostgreSqlTableColumn("MethodId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("MaxStack", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("MethodId"), methodTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val methodMaxLocalsValueTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_Method_MaxLocals",
    columns = List(
      PostgreSqlTableColumn("MethodId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("MaxLocals", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("MethodId"), methodTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id"))
    )
  )

  val fieldDefinitionFlagValueTable: PostgreSqlTable = new PostgreSqlTable(tableName = "Rel_FieldDefinitions_AccessFlags",
    columns = List(
      PostgreSqlTableColumn("FieldDefinitionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("VersionId", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int)
    ), fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("FieldDefinitionId"), fieldDefinitionTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("VersionId"), versionNumberTable.tableName, List("Id")),
      PostgreSqlForeignKeyConstraint(List("Flags"), accessFlagsTable.tableName, List("Flags"))
    )
  )


}

class PostgreSqlTable (val tableName: String,
                       val columns: List[PostgreSqlTableColumn],
                       val autoIdColumn: Boolean = false,
                       val fkConstraints: List[PostgreSqlForeignKeyConstraint] = List.empty) {

  private val theColumns = {
    if(autoIdColumn){
      List(PostgreSqlTableColumn.autoIdColumn) ++ columns
    } else {
      columns
    }
  }

  def tableDefinitionSql: String = {
    val builder = new StringBuilder("CREATE TABLE IF NOT EXISTS ")
    builder.append(tableName)
    builder.append("( ")

    val columnIt = theColumns.iterator

    while(columnIt.hasNext){
      val column = columnIt.next()

      builder.append(column.toColumnDef)

      if(columnIt.hasNext){
        builder.append(", ")
      }
    }

    if(theColumns.exists(_.isPrimaryKey)){
      builder.append(", ")
      builder.append(buildPrimaryKeyConstraintSql)
    }

    fkConstraints.foreach { fkc =>
      builder.append(", ")
      builder.append(fkc.toConstraintSql)
    }

    builder.append(" );")

    builder.toString()
  }

  def buildInsertStmt(connection: Connection, ignoreConflicts: Boolean): PreparedStatement =
    connection.prepareStatement(buildInsertSql(ignoreConflicts))

  def buildInsertSql(ignoreConflicts: Boolean): String = {
    val builder = new StringBuilder("INSERT INTO ")
    builder.append(tableName)
    builder.append(" (")

    // Always ignore auto id column, we never want to insert a value there
    val columnHeaderIt = columns.iterator

    while(columnHeaderIt.hasNext){
      builder.append(columnHeaderIt.next().columnName)

      if(columnHeaderIt.hasNext) builder.append(", ")
    }


    builder.append(") ")
    builder.append(" VALUES( ")

    val columnIt = columns.iterator

    while(columnIt.hasNext) {
      columnIt.next()
      builder.append("?")

      if(columnIt.hasNext){
        builder.append(", ")
      }
    }

    builder.append(" )")

    if(ignoreConflicts){
      builder.append(" ON CONFLICT DO NOTHING")
    }

    builder.append(";")

    builder.toString()
  }

  private def buildPrimaryKeyConstraintSql: String = {
    val builder = new StringBuilder("CONSTRAINT ")

    builder.append("PK_")
    builder.append(tableName)
    builder.append(" PRIMARY KEY ( ")

    val columnNamesIt = theColumns.filter(_.isPrimaryKey).map(_.columnName).iterator

    while(columnNamesIt.hasNext){
      builder.append(columnNamesIt.next())

      if(columnNamesIt.hasNext){
        builder.append(", ")
      }
    }

    builder.append(" )")

    builder.toString()
  }

}

case class PostgreSqlTableColumn (columnName: String, dataType: PostgreSqlDataType, isPrimaryKey: Boolean = false, isNullable: Boolean = false){

  def toColumnDef: String = {
    val builder = new StringBuilder(columnName)
    builder.append(" ")
    builder.append(dataType.toString)

    if(!isNullable){
      builder.append(" NOT NULL")
    }

    builder.toString()
  }

}

object PostgreSqlTableColumn {

  val autoIdColumn: PostgreSqlTableColumn =
    PostgreSqlTableColumn("Id", PostgreSqlDataTypes.AutoId, isPrimaryKey = true)
}

case class PostgreSqlForeignKeyConstraint (localColumnNames: List[String], targetTableName: String, targetColumnNames: List[String]) {

  def toConstraintSql: String = {
    val builder = new StringBuilder("CONSTRAINT FK_")
    builder.append(targetTableName)
    builder.append("_")

    targetColumnNames.foreach { name =>
      builder.append(name)
    }

    builder.append(" FOREIGN KEY (")

    val localNameIt = localColumnNames.iterator

    while(localNameIt.hasNext){
      builder.append(localNameIt.next())

      if(localNameIt.hasNext){
        builder.append(", ")
      }
    }

    builder.append(") REFERENCES ")
    builder.append(targetTableName)
    builder.append("(")

    val targetNameIt = targetColumnNames.iterator

    while(targetNameIt.hasNext){
      builder.append(targetNameIt.next())

      if(targetNameIt.hasNext){
        builder.append(", ")
      }
    }

    builder.append(")")

    builder.toString()
  }

}

object PostgreSqlDataTypes extends Enumeration {

  type PostgreSqlDataType = Value

  val Text: Value = Value("VARCHAR(1024)")
  val ShortText: Value = Value("VARCHAR(255)")
  val Int: Value = Value("Int")
  val AutoId: Value = Value("SERIAL")
  val Bool: Value = Value("BOOLEAN")

}
