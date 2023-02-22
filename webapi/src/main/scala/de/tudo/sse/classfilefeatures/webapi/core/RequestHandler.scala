package de.tudo.sse.classfilefeatures.webapi.core

import de.tudo.sse.classfilefeatures.webapi.WebapiConfig
import de.tudo.sse.classfilefeatures.webapi.model.requests.ExecuteAnalysisRequest
import de.tudo.sse.classfilefeatures.webapi.model.{AnalysisResultRepr, AnalysisRunRepr, EntityRepr, genericEntityToEntityRepr, toEntityRepr, toResultRepr, toRunRepr}
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqMessageWriter
import de.tudo.sse.spareuse.core.model.{RunState, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import de.tudo.sse.spareuse.core.storage.DataAccessor
import org.slf4j.{Logger, LoggerFactory}
import spray.json.{JsonWriter, enrichAny}

import scala.util.{Failure, Success, Try}

class RequestHandler(val configuration: WebapiConfig, dataAccessor: DataAccessor) extends RunnerCommandJsonSupport {

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

  def getAllResultsFor(entityName: String, analysisFilter: Option[String], limit: Int, skip: Int): Try[Set[AnalysisResultRepr]] = {
    val analysisNameAndVersionOpt = analysisFilter.map( s => {
      val parts = s.split(":")
      (parts(0).trim, parts(1).trim)
    })

    dataAccessor
      .getJSONResultsFor(entityName, analysisNameAndVersionOpt, limit, skip)
      .map(results => results.map(toResultRepr))
  }

  def getRun(analysisName: String, analysisVersion: String, runId: String): Try[AnalysisRunRepr] = {
    dataAccessor
      .getAnalysisRun(analysisName, analysisVersion, runId, includeResults = false)
      .map(toRunRepr)
  }

  def getRunIdIfPresent(analysisName: String, analysisVersion: String, request: ExecuteAnalysisRequest): Try[Option[String]] = {
    dataAccessor.getAnalysisRuns(analysisName, analysisVersion) match {
      case Success(runs) =>
        Success(
          runs.find{ r =>
            r.configuration.equals(request.Configuration) &&
            r.inputs.map(_.uid).equals(request.Inputs.toSet) &&
            r.state.equals(RunState.Finished)
          }.map(_.uid)
        )
      case Failure(ex) =>
        log.error(s"Failed to retrieve runs from DB for $analysisName:$analysisVersion", ex)
        Failure(ex)
    }
  }

  def triggerNewAnalysisRun(analysisName: String, analysisVersion: String, request: ExecuteAnalysisRequest): Try[String] = Try {

    // Create new empty record for analysis run
    val newId = dataAccessor.storeEmptyAnalysisRun(analysisName, analysisVersion, request.Configuration, request.Inputs.toSet).get

    // Queue run execution
    val name = s"$analysisName:$analysisVersion"
    val command = RunnerCommand(name, newId, request.User.getOrElse("Anonymous"), request.Inputs.toSet, request.Configuration)

    val writer = new MqMessageWriter(configuration.asAnalysisQueuePublishConfig)
    writer.initialize()
    writer.appendToQueue(command.toJson.compactPrint)
    writer.shutdown()

    newId

  }

  def getRunResults(runId: String, limit: Int, skip: Int): Try[Set[AnalysisResultRepr]] = {
    dataAccessor.getRunResultsAsJSON(runId, skip, limit).map { allResults =>
      allResults.map(result => toResultRepr(result))
    }
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
