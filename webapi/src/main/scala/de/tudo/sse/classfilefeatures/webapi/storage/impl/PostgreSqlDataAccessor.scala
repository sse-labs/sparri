package de.tudo.sse.classfilefeatures.webapi.storage.impl

import de.tudo.sse.classfilefeatures.common.storage.impl.postgresql.{PostgreSqlConnectionConfiguration, PostgreSqlConnectivity}
import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor

import java.sql.PreparedStatement
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
    val stmt = connection.prepareStatement("SELECT Classfiles.ThisType FROM Libraries " +
      "LEFT JOIN Classfiles ON Libraries.Id = Classfiles.LibraryId " +
      "WHERE Libraries.LibraryName = ?")

    stmt.setString(1, libraryName)

    resultsAsStringArray(stmt)
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


  private def resultsAsStringArray(stmt: PreparedStatement): Array[String] = {

    val rs = stmt.executeQuery()

    val stringValues = new mutable.HashSet[String]

    while(rs.next()) { stringValues.add(rs.getString(1)) }

    rs.close()
    stmt.close()

    stringValues.toArray
  }
}
