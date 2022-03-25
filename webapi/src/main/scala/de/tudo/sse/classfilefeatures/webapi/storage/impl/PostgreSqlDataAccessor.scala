package de.tudo.sse.classfilefeatures.webapi.storage.impl

import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.{PostgreSqlConnectionConfiguration, PostgreSqlConnectivity}
import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor

import scala.collection.mutable

class PostgreSqlDataAccessor(override protected val connectionConfiguration: PostgreSqlConnectionConfiguration)
  extends PostgreSqlConnectivity with ClassfileDataAccessor {

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
    val rs = stmt.executeQuery()

    val libraryPresent = rs.next()

    stmt.close()

    libraryPresent
  }

  override def getLibraryNames(offset: Int, count: Int): Array[String] = {

    //TODO: Pagination
    val stmt = connection.prepareStatement("SELECT LibraryName FROM Libraries;")
    val rs = stmt.executeQuery()

    val names = new mutable.ListBuffer[String]

    while(rs.next()){
      names.append(rs.getString(1))
    }

    stmt.close()
    names.toArray
  }
}
