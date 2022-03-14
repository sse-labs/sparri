package de.tudo.sse.classfilefeatures.extractor.storage.impl

import de.tudo.sse.classfilefeatures.extractor.model.LibraryClassfileFeatureModel
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler

import scala.util.{Success, Try}

class PostgreSqlStorageHandler(config: PostgreSqlConnectionConfiguration) extends ClassfileFeatureStorageHandler {

  override def verifyConnectivity(): Unit = {} //TODO: IMPLEMENT

  override def isLibraryPresent(libraryIdentifier: String): Boolean = false // TODO: Implement

  override def storeLibraryFeatureModel(model: LibraryClassfileFeatureModel): Try[Unit] = {
    log.info(s"Storing ${model.libraryIdentifier}")
    Success()
  } //TODO: Implement

  override def shutdown(): Unit = {} //TODO: IMPLEMENT
}

trait PostgreSqlConnectionConfiguration {
  //TODO: Specify connection parameters
}
