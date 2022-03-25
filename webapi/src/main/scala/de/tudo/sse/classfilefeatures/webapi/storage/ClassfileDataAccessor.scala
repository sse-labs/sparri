package de.tudo.sse.classfilefeatures.webapi.storage

trait ClassfileDataAccessor {

  def verifyConnectivity(): Unit

  def shutdown(): Unit

  def hasLibrary(libraryName: String): Boolean

  def getLibraryNames(offset: Int = 0, count: Int = 100): Array[String]

}
