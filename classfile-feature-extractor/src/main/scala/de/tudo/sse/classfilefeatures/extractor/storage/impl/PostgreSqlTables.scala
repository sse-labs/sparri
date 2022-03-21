package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlDataTypes.PostgreSqlDataType

import java.sql.{Connection, PreparedStatement}

object PostgreSqlTables {

  val libraryTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Libraries",
    columns = List(
      PostgreSqlTableColumn("LibraryIdentifier", PostgreSqlDataTypes.Text, isPrimaryKey = true)
    ))

  val versionNumberTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "VersionNumbers",
    columns = List(
     PostgreSqlTableColumn("VersionNumber", PostgreSqlDataTypes.ShortText, isPrimaryKey = true)
    ))

  val accessFlagsTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "AccessFlags",
    columns = List(
      PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int, isPrimaryKey = true)
    ))

  //TODO: Attributes, Methods
  val classFileTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Classfiles",
    columns = List(
      PostgreSqlTableColumn("ThisType", PostgreSqlDataTypes.Text, isPrimaryKey = true),
      PostgreSqlTableColumn("Library", PostgreSqlDataTypes.Text, isPrimaryKey = true)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("Library"), libraryTable.tableName, List("LibraryIdentifier"))
    )
  )

  val fieldDefinitionTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "FieldDefinitions",
    columns = List(
      PostgreSqlTableColumn("CfThisType", PostgreSqlDataTypes.Text, isPrimaryKey = true),
      PostgreSqlTableColumn("CfLibrary", PostgreSqlDataTypes.Text, isPrimaryKey = true),
      PostgreSqlTableColumn("FieldName", PostgreSqlDataTypes.Text, isPrimaryKey = true),
      PostgreSqlTableColumn("FieldType", PostgreSqlDataTypes.Text, isPrimaryKey = true)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("CfThisType", "CfLibrary"), classFileTable.tableName, List("ThisType", "Library"))
    )
  )

  val libraryToVersionsTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Rel_Libraries_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("Library", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("Version", PostgreSqlDataTypes.ShortText)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("Library"), libraryTable.tableName, List("LibraryIdentifier")),
      PostgreSqlForeignKeyConstraint(List("Version"), versionNumberTable.tableName, List("VersionNumber"))
    )
  )

  val classFilesToVersionsTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Rel_Classfiles_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("CfThisType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("CfLibrary", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("Version", PostgreSqlDataTypes.ShortText)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("CfThisType", "CfLibrary"), classFileTable.tableName, List("ThisType", "Library")),
      PostgreSqlForeignKeyConstraint(List("Version"), versionNumberTable.tableName, List("VersionNumber"))
    )
  )

  val classFilesToFlagsTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Rel_Classfiles_AccessFlags",
    columns = List(
      PostgreSqlTableColumn("CfThisType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("CfLibrary", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("Version", PostgreSqlDataTypes.ShortText)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("CfThisType", "CfLibrary"), classFileTable.tableName, List("ThisType", "Library")),
      PostgreSqlForeignKeyConstraint(List("Flags"), accessFlagsTable.tableName, List("Flags")),
      PostgreSqlForeignKeyConstraint(List("Version"), versionNumberTable.tableName, List("VersionNumber"))
    )
  )

  val fieldDefinitionsToVersionsTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Rel_FieldDefinitions_VersionNumbers",
    columns = List(
      PostgreSqlTableColumn("CfThisType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("CfLibrary", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("FdName", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("FdType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("Version", PostgreSqlDataTypes.ShortText)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("CfThisType", "CfLibrary", "FdName", "FdType"), fieldDefinitionTable.tableName, List("CfThisType", "CfLibrary", "FieldName", "FieldType")),
      PostgreSqlForeignKeyConstraint(List("Version"), versionNumberTable.tableName, List("VersionNumber"))
    )
  )

  val fieldDefinitionsToFlagsTable: PostgreSqlTable = new PostgreSqlTable(
    tableName = "Rel_FieldDefinitions_AccessFlags",
    columns = List(
      PostgreSqlTableColumn("CfThisType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("CfLibrary", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("FdName", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("FdType", PostgreSqlDataTypes.Text),
      PostgreSqlTableColumn("Flags", PostgreSqlDataTypes.Int),
      PostgreSqlTableColumn("Version", PostgreSqlDataTypes.ShortText)
    ),
    fkConstraints = List(
      PostgreSqlForeignKeyConstraint(List("CfThisType", "CfLibrary", "FdName", "FdType"), fieldDefinitionTable.tableName, List("CfThisType", "CfLibrary", "FieldName", "FieldType")),
      PostgreSqlForeignKeyConstraint(List("Flags"), accessFlagsTable.tableName, List("Flags")),
      PostgreSqlForeignKeyConstraint(List("Version"), versionNumberTable.tableName, List("VersionNumber"))
    )
  )

}

class PostgreSqlTable (val tableName: String,
                       val columns: List[PostgreSqlTableColumn],
                       val fkConstraints: List[PostgreSqlForeignKeyConstraint] = List.empty) {

  def tableDefinitionSql: String = {
    val builder = new StringBuilder("CREATE TABLE IF NOT EXISTS ")
    builder.append(tableName)
    builder.append("( ")

    val columnIt = columns.iterator

    while(columnIt.hasNext){
      val column = columnIt.next()

      builder.append(column.toColumnDef)

      if(columnIt.hasNext){
        builder.append(", ")
      }
    }

    if(columns.exists(_.isPrimaryKey)){
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

    val columnNamesIt = columns.filter(_.isPrimaryKey).map(_.columnName).iterator

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

}
