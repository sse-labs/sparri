package de.tudo.sse.classfilefeatures.webapi.storage

import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation
import de.tudo.sse.classfilefeatures.webapi.storage.model.LibraryClassInformationStorageModel

trait ClassfileDataAccessor {

  def verifyConnectivity(): Unit

  def shutdown(): Unit

  def hasLibrary(libraryName: String): Boolean

  def hasRelease(libraryName: String, releaseName: String): Boolean

  def hasLibraryClass(libraryName: String, className: String): Boolean

  def hasReleaseClass(libraryName: String, releaseName: String, className: String): Boolean


  def getLibraryNames(offset: Int = 0, count: Int = 100): Array[String]

  def getLibraryClassNames(libraryName: String): Array[String]

  def getLibraryClassActivationInformation(libraryName: String): Array[(String, Set[String])]

  def getLibraryClassInformation(libraryName: String, className: String): LibraryClassInformationStorageModel

  def getReleaseNames(libraryName: String): Array[String]

  def getReleaseClassNames(libraryName: String, releaseName: String): Array[String]

  def getClassRepresentation(libraryName: String, releaseName: String, className: String): ClassFileRepresentation


}
