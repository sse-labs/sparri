package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassInterfaces, JavaClasses, JavaFieldAccessStatements, JavaInvocationStatements, JavaMethodDescriptors, JavaMethods, JavaPrograms, JavaTypeNames}
import slick.lifted.TableQuery

trait PostgresEntityTables {

  protected val entitiesTable = TableQuery[SoftwareEntities]
  protected val typeNameTable = TableQuery[JavaTypeNames]
  protected val descriptorTable = TableQuery[JavaMethodDescriptors]
  protected val javaProgramsTable = TableQuery[JavaPrograms]
  protected val javaClassesTable = TableQuery[JavaClasses]
  protected val javaClassInterfacesTable = TableQuery[JavaClassInterfaces]
  protected val javaMethodsTable = TableQuery[JavaMethods]
  protected val javaInvocationsTable = TableQuery[JavaInvocationStatements]
  protected val javaFieldAccessesTable = TableQuery[JavaFieldAccessStatements]

  protected lazy val allEntityTables = Seq(entitiesTable, typeNameTable, descriptorTable, javaProgramsTable,
    javaClassesTable, javaClassInterfacesTable, javaMethodsTable, javaInvocationsTable, javaFieldAccessesTable)

}
