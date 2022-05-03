package de.tudo.sse.classfilefeatures.webapi.core

import akka.NotUsed
import akka.stream.scaladsl.Source
import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation
import de.tudo.sse.classfilefeatures.common.model.conversion.ClassfileCreators
import de.tudo.sse.classfilefeatures.common.rabbitmq.MqMessageWriter
import de.tudo.sse.classfilefeatures.webapi.Configuration
import de.tudo.sse.classfilefeatures.webapi.model.{ConcreteClassInformation, ConcreteClassInformationBuilder, ConditionallyActiveElementEntry, LibraryClassActivationInformation, LibraryClassInformation, LibraryInformation, ReleaseInformation}
import de.tudo.sse.classfilefeatures.webapi.storage.ClassfileDataAccessor
import org.opalj.bc.Assembler
import org.slf4j.{Logger, LoggerFactory}

import java.io.ByteArrayOutputStream
import java.util.jar.{JarEntry, JarOutputStream}
import scala.util.{Failure, Success, Try}

class RequestHandler(val configuration: Configuration, dataAccessor: ClassfileDataAccessor){

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val existingResourcesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()
  private val classFileCache: ObjectCache[ClassFileRepresentation] = new ObjectCache[ClassFileRepresentation](5000)

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
      dataAccessor.getLibraryClassActivationInformation(libraryName).map(_._1))
  }

  def getLibraryClassActivationInformation(libraryName: String): Array[LibraryClassActivationInformation] = {
    val totalReleaseCountForLibrary = dataAccessor.getReleaseNames(libraryName).length

    dataAccessor
      .getLibraryClassActivationInformation(libraryName)
      .map{ tuple =>
        val alwaysActive = tuple._2.size == totalReleaseCountForLibrary

        LibraryClassActivationInformation(tuple._1, if(alwaysActive) Array.empty[String] else tuple._2.toArray, alwaysActive)
      }
  }

  def getLibraryClassInformation(libraryName: String, className: String): LibraryClassInformation = {
    val infoObj = dataAccessor.getLibraryClassInformation(libraryName, className)

    def toConditionallyActiveEntryObj(a: Array[String]): ConditionallyActiveElementEntry ={
      val alwaysActive = a.length == infoObj.activeIn.length
      ConditionallyActiveElementEntry( if(alwaysActive) Array.empty[String] else a, alwaysActive)
    }

    LibraryClassInformation(infoObj.className,
      infoObj.activeIn,
      infoObj.supertypes.mapValues( toConditionallyActiveEntryObj ),
      infoObj.flags.map(t => (String.valueOf(t._1), toConditionallyActiveEntryObj(t._2))),
      infoObj.methodNames.mapValues( toConditionallyActiveEntryObj ))

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
    val classRep = classFileCache.getWithCache( libraryName + releaseName + className,
      () => dataAccessor.getClassRepresentation(libraryName, releaseName, className))

    val dummyClassFile = ClassfileCreators.buildSimpleCreator(Seq(classRep)).toDummyClassFile(classRep)

    val classBytes = Assembler(org.opalj.ba.toDA(dummyClassFile))

    Source.fromIterator(() => classBytes.iterator)
  }

  def getJar(libraryName: String, releaseName: String): Source[Byte, NotUsed] = {

    log.info(s"Starting to build dummy JAR for $libraryName:$releaseName. Retrieving data...")

    val allClassReps = getReleaseInfo(libraryName, releaseName)
      .classNames
      .map(className => classFileCache.getWithCache( libraryName + releaseName + className,
        () => dataAccessor.getClassRepresentation(libraryName, releaseName, className)))

    log.info("Done retrieving data. Building JAR ...")
    val classFileCreator = ClassfileCreators.buildCreatorWithAiSupport(allClassReps)

    val byteStream = new ByteArrayOutputStream()
    val jarStream = new JarOutputStream(byteStream)

    allClassReps.foreach { cfr =>

      val brClassObj = classFileCreator.toDummyClassFile(cfr)
      val classBytes = Assembler(org.opalj.ba.toDA(brClassObj))

      val entryName = brClassObj.thisType.packageName + "/" + brClassObj.thisType.simpleName + ".class"
      val jarEntry = new JarEntry(entryName)
      jarEntry.setTime( System.currentTimeMillis / 1000)

      jarStream.putNextEntry(jarEntry)
      jarStream.write(classBytes)
      jarStream.closeEntry()

    }

    jarStream.close()
    val jarBytes = byteStream.toByteArray

    log.info("Done building JAR.")

    //TODO: Cache calculates JARs!

    Source.fromIterator(() => jarBytes.iterator)
  }

  def processEnqueueLibraryRequest(libraryName: String): Boolean = {

    Try {
      val writer = new MqMessageWriter(configuration)
      writer.initialize()

      writer.appendToQueue(libraryName)

      writer.shutdown()
    } match {
      case Success(_) => true
      case Failure(ex) =>
        log.error(s"Failed to enqueue library $libraryName", ex)
        false
    }


  }


}
