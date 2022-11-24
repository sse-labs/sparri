package de.tudo.sse.classfilefeatures.webapi.core

import de.tudo.sse.classfilefeatures.webapi.WebapiConfig
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqMessageWriter
import de.tudo.sse.classfilefeatures.webapi.storage.WebapiDataAccessor
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

class RequestHandler(val configuration: WebapiConfig, dataAccessor: WebapiDataAccessor){

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val existingResourcesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()


  def hasLibrary(libraryName: String): Boolean = {
    existingResourcesCache.getWithCache(libraryName, () => dataAccessor.hasEntity(libraryName, SoftwareEntityKind.Library))
  }

  def processEnqueueLibraryRequest(libraryName: String): Boolean = {

    Try {
      val writer = new MqMessageWriter(configuration.asMinerQueuePublishConfig)
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
