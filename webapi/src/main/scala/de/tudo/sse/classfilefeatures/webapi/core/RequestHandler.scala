package de.tudo.sse.classfilefeatures.webapi.core

import de.tudo.sse.classfilefeatures.webapi.WebapiConfig
import de.tudo.sse.classfilefeatures.webapi.model.{EntityRepr, genericEntityToEntityRepr, toEntityRepr}
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqMessageWriter
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.storage.DataAccessor
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

class RequestHandler(val configuration: WebapiConfig, dataAccessor: DataAccessor){

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val existingResourcesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()

  private val existingEntitiesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()
  private val existingAnalysesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()


  def hasLibrary(libraryName: String): Boolean = {
    existingResourcesCache.getWithCache(libraryName, () => dataAccessor.hasEntity(libraryName, SoftwareEntityKind.Library))
  }

  def hasEntity(entityName: String): Boolean = {
    existingEntitiesCache.getWithCache(entityName, () => dataAccessor.hasEntity(entityName))
  }

  def hasAnalysis(analysisName: String, version: Option[String] = None): Boolean = {
    val key = if(version.isDefined) s"$analysisName:${version.get}" else analysisName

    existingAnalysesCache.getWithCache(key, () => {
      if(version.isDefined) dataAccessor.hasAnalysis(analysisName, version.get)
      else dataAccessor.hasAnalysis(analysisName)
    })
  }

  def hasAnalysisRun(analysisName: String, version: String, runUid: String): Boolean = {
    val key = s"$analysisName:$version:$runUid"

    existingAnalysesCache.getWithCache(key, () => dataAccessor.hasAnalysisRun(analysisName, version, runUid))
  }

  def getAllEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[String]): Try[Seq[EntityRepr]] = {
    dataAccessor.getEntities(limit, skip, kindFilter, parentFilter) match {
      case Success(entityDataList) =>
        Success(entityDataList.map(genericEntityToEntityRepr))
      case Failure(ex) =>
        log.error("Database connection error while retrieving entities", ex)
        Failure(ex)
    }
  }

  def getEntity(entityName: String): Try[EntityRepr] = {
    dataAccessor.getEntity(entityName).map(toEntityRepr)
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
