package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClasses, JavaFieldAccessStatements, JavaInvocationStatements, JavaMethods, JavaPrograms}
import slick.lifted.TableQuery

trait PostgresEntityTables {

  protected val entitiesTable = TableQuery[SoftwareEntities]
  protected val javaProgramsTable = TableQuery[JavaPrograms]
  protected val javaClassesTable = TableQuery[JavaClasses]
  protected val javaMethodsTable = TableQuery[JavaMethods]
  protected val javaInvocationsTable = TableQuery[JavaInvocationStatements]
  protected val javaFieldAccessesTable = TableQuery[JavaFieldAccessStatements]

  protected lazy val allEntityTables = Seq(entitiesTable, javaProgramsTable, javaClassesTable, javaMethodsTable,
    javaInvocationsTable, javaFieldAccessesTable)

}
