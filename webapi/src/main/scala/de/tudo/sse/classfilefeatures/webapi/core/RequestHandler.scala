package de.tudo.sse.classfilefeatures.webapi.core

import de.tudo.sse.classfilefeatures.webapi.WebapiConfig
import de.tudo.sse.classfilefeatures.webapi.model.requests.ExecuteAnalysisRequest
import de.tudo.sse.classfilefeatures.webapi.model.{AnalysisInformationRepr, AnalysisResultFormatRepr, AnalysisResultRepr, AnalysisRunRepr, EntityRepr, genericEntityToEntityRepr, toAnalysisFormatRepr, toAnalysisRepr, toEntityRepr, toResultRepr, toRunRepr}
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqMessageWriter
import de.tudo.sse.spareuse.core.model.RunState
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import de.tudo.sse.spareuse.core.model.entities.{MinerCommand, MinerCommandJsonSupport}
import de.tudo.sse.spareuse.core.storage.DataAccessor
import org.slf4j.{Logger, LoggerFactory}
import spray.json.enrichAny

import scala.util.{Failure, Success, Try}

class RequestHandler(val configuration: WebapiConfig, dataAccessor: DataAccessor) extends RunnerCommandJsonSupport with MinerCommandJsonSupport {

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val existingEntitiesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()
  private val existingAnalysesCache: SimpleValueCache[Boolean] = new SimpleValueCache[Boolean]()

  def hasEntity(entityName: String): Boolean = {
    dataAccessor.hasEntity(entityName)
  }

  def getAnalyses(limit: Int, skip: Int): Try[Set[AnalysisInformationRepr]] = {
    dataAccessor
      .getAnalyses(includeRuns = true, skip, limit)
      .map(allAnalyses => allAnalyses.map(toAnalysisRepr))
  }

  def getAnalyses(analysisName: String): Try[Set[AnalysisInformationRepr]] = {
    dataAccessor
      .getAnalysesFor(analysisName, includeRuns = true)
      .map(allAnalyses => allAnalyses.map(toAnalysisRepr))
  }

  def getAnalysis(analysisName: String, analysisVersion: String): Try[AnalysisInformationRepr] = {
    dataAccessor
      .getAnalysisData(analysisName, analysisVersion, includeRuns = true)
      .map(toAnalysisRepr)
  }

  def getAnalysisResultFormat(analysisName: String, analysisVersion: String): Try[AnalysisResultFormatRepr] = {
    dataAccessor
      .getAnalysisData(analysisName, analysisVersion)
      .map(toAnalysisFormatRepr)
  }

  def getAnalysisRuns(analysisName: String, analysisVersion: String, limit: Int, skip: Int): Try[Set[AnalysisRunRepr]] = {
    dataAccessor
      .getAnalysisRuns(analysisName, analysisVersion, includeResults = false, skip, limit)
      .map(allRuns => allRuns.map(toRunRepr))
  }

  def getAnalysisRunsForEntity(entityName: String, limit: Int, skip: Int): Try[Set[AnalysisRunRepr]] = {
    dataAccessor
      .getAnalysisRunsForEntity(entityName, skip, limit)
      .map(allRuns => allRuns.map(toRunRepr))
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

  def getEntityChildren(entityName: String, skip: Int, limit: Int): Try[Seq[EntityRepr]] = Try {
    dataAccessor.getEntityChildren(entityName, skip, limit).get.map(toEntityRepr)
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
      .getAnalysisRun(analysisName, analysisVersion, runId)
      .map(toRunRepr)
  }

  def getRunIdIfPresent(analysisName: String, analysisVersion: String, request: ExecuteAnalysisRequest): Try[Option[String]] = {
    dataAccessor.getAnalysisRuns(analysisName, analysisVersion) match {
      case Success(runs) =>
        val requestInputs = request.Inputs.toSet
        Success(
          runs.find{ r =>

            val runInputs = r.inputs.map(_.uid)

            r.configuration.equals(request.Configuration) &&
            runInputs.equals(requestInputs) &&
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
    val newId = dataAccessor.storeEmptyAnalysisRun(analysisName, analysisVersion, request.Configuration).get

    // Queue run execution
    val name = s"$analysisName:$analysisVersion"
    val command = RunnerCommand(name, newId, request.User.getOrElse("Anonymous"), request.Inputs.toSet, request.Configuration)
    val commandJson = command.toJson.compactPrint

    log.info(s"Publishing a new Runner Command for analysis $name : $commandJson")

    val writer = new MqMessageWriter(configuration.asAnalysisQueuePublishConfig)
    writer.initialize()
    writer.appendToQueue(commandJson)
    writer.shutdown()

    newId

  }

  def getRunResults(runId: String, limit: Int, skip: Int): Try[Set[AnalysisResultRepr]] = {
    dataAccessor.getRunResultsAsJSON(runId, skip, limit).map { allResults =>
      allResults.map(result => toResultRepr(result))
    }
  }

  def triggerEntityMining(entityIdent: String): Boolean = {

    Try {
      val writer = new MqMessageWriter(configuration.asMinerQueuePublishConfig)
      writer.initialize()

      val msg = MinerCommand(Set(entityIdent), None).toJson.compactPrint

      log.info(s"Publishing a new Miner Command for entity $entityIdent : $msg")

      writer.appendToQueue(msg, Some(3))

      writer.shutdown()
    } match {
      case Success(_) => true
      case Failure(ex) =>
        log.error(s"Failed to enqueue entity id $entityIdent", ex)
        false
    }


  }


}
