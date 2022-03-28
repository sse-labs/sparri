package de.tudo.sse.classfilefeatures.webapi.storage

trait ClassfileDataAccessor {

  def verifyConnectivity(): Unit

  def shutdown(): Unit

  def hasLibrary(libraryName: String): Boolean

  def hasRelease(libraryName: String, releaseName: String): Boolean

  def hasLibraryClass(libraryName: String, className: String): Boolean

  def hasReleaseClass(libraryName: String, releaseName: String, className: String): Boolean


  def getLibraryNames(offset: Int = 0, count: Int = 100): Array[String]

  def getLibraryClassNames(libraryName: String): Array[String]

  def getReleaseNames(libraryName: String): Array[String]

  def getReleaseClassNames(libraryName: String, releaseName: String): Array[String]


}
