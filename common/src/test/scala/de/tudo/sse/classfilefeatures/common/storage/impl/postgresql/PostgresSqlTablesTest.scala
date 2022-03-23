package de.tudo.sse.classfilefeatures.common.storage.impl.postgresql

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must

class PostgresSqlTablesTest extends AnyFlatSpec with must.Matchers {


  "The table objects" must "produce valid definition statements" in {
    val sql = PostgreSqlTables.classFileTable.tableDefinitionSql

    println(sql)
  }

  "The table objects" must "produce valid insertion statements" in {
    val sql = PostgreSqlTables.libraryTable.buildInsertSql(true)

    println(sql)
  }
}
