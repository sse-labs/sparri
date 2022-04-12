package de.tudo.sse.classfilefeatures.webapi.core

import akka.NotUsed
import akka.stream.scaladsl.Source
import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation
import de.tudo.sse.classfilefeatures.common.model.conversion.ClassfileCreators
import de.tudo.sse.classfilefeatures.webapi.model.{ConcreteClassInformation, ConcreteClassInformationBuilder, LibraryInformation, ReleaseInformation}
import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor
import org.opalj.bc.Assembler

class RequestHandler(dataAccessor: ClassfileDataAccessor){

  private val existingResourcesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()

  def getLibraries(offset: Int = 0, count: Int = 100): Array[String] =
    dataAccessor.getLibraryNames(offset, count)

  def hasLibrary(libraryName: String): Boolean = {
    existingResourcesCache.getWithCache(libraryName, () => dataAccessor.hasLibrary(libraryName))
  }

  def hasRelease(libraryName: String, version: String): Boolean = {
    existingResourcesCache.getWithCache(libraryName + ":" + version, () => dataAccessor.hasRelease(libraryName, version))
  }

  def hasLibraryClass(libraryName: String, className: String): Boolean = {
    existingResourcesCache.getWithCache(libraryName + "/" + className, () => dataAccessor.hasLibraryClass(libraryName, className))
  }

  def hasReleaseClass(libraryName: String, releaseName: String, className: String): Boolean = {
    val ident = libraryName + ":" + releaseName + "/" + className
    existingResourcesCache.getWithCache(ident, () => dataAccessor.hasReleaseClass(libraryName, releaseName, className))
  }

  def getLibraryInfo(libraryName: String): LibraryInformation = {
    LibraryInformation(libraryName,
      dataAccessor.getReleaseNames(libraryName),
      dataAccessor.getLibraryClassNames(libraryName))
  }

  def getReleaseInfo(libraryName: String, releaseName: String): ReleaseInformation = {
    ReleaseInformation(libraryName,
      releaseName,
      dataAccessor.getReleaseClassNames(libraryName, releaseName))
  }

  def getClassInfo(libraryName: String, releaseName: String, className: String): ConcreteClassInformation = {
    ConcreteClassInformationBuilder.fromRepresentation(
      libraryName,
      releaseName,
      dataAccessor.getClassRepresentation(libraryName, releaseName, className)
    )
  }

  def getSingleClassFile(libraryName: String, releaseName: String, className: String): Source[Byte, NotUsed] = {
    val classRep = dataAccessor.getClassRepresentation(libraryName, releaseName, className)
    val dummyClassFile = ClassfileCreators.buildSimpleCreator(Seq(classRep)).toDummyClassFile(classRep)

    val classBytes = Assembler(org.opalj.ba.toDA(dummyClassFile))

    Source.fromIterator(() => classBytes.iterator)
  }


}
