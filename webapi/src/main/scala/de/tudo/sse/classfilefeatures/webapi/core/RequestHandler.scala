package de.tudo.sse.classfilefeatures.webapi.core

import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor

class RequestHandler(dataAccessor: ClassfileDataAccessor){

  def getLibraries(offset: Int = 0, count: Int = 100): Array[String] =
    dataAccessor.getLibraryNames(offset, count)

  def hasLibrary(libraryName: String): Boolean =
    dataAccessor.hasLibrary(libraryName)

  def releasesForLibrary(libraryName: String): Array[String] = Array.empty

  def hasRelease(libraryName: String, version: String): Boolean = //IMPROVE: Use dedicated query to increase performance
    hasLibrary(libraryName) && releasesForLibrary(libraryName).contains(version)

}
