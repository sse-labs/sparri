package de.tudo.sse.classfilefeatures.extractor.storage

import de.tudo.sse.classfilefeatures.extractor.model.LibraryClassfileFeatureModel
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

trait ClassfileFeatureStorageHandler {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  def verifyConnectivity(): Unit

  def initialize(): Unit

  def isLibraryPresent(libraryIdentifier: String): Boolean

  def storeLibraryFeatureModel(model: LibraryClassfileFeatureModel): Try[Unit]

  def shutdown(): Unit

}
