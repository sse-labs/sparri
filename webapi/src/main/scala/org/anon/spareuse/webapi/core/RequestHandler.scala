package org.anon.spareuse.webapi.core

import org.anon.spareuse.webapi.model.{toAnalysisFormatRepr, toAnalysisRepr, toEntityRepr, toResultRepr, toRunRepr}
import org.anon.spareuse.core.utils.rabbitmq.MqMessageWriter
import org.anon.spareuse.core.model.{RunState, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.analysis.{AnalysisCommand, RunnerCommandJsonSupport}
import org.anon.spareuse.core.model.entities.{MinerCommand, MinerCommandJsonSupport}
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.webapi.WebapiConfig
import org.anon.spareuse.webapi.model.requests.ExecuteAnalysisRequest
import org.anon.spareuse.webapi.model.{AnalysisInformationRepr, AnalysisResultFormatRepr, AnalysisResultRepr, AnalysisRunRepr, EntityRepr}
import org.slf4j.{Logger, LoggerFactory}
import spray.json.enrichAny

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class RequestHandler(val configuration: WebapiConfig, dataAccessor: DataAccessor)(implicit context: ExecutionContext) extends RunnerCommandJsonSupport with MinerCommandJsonSupport {

  private val log: Logger = LoggerFactory.getLogger(getClass)

  def hasEntity(eid: Long): Boolean = {
    dataAccessor.hasEntity(eid)
  }

  def validIndexEntity(ident: String): Boolean = {
    val parts = ident.split(":")

    parts.size match {
      case 2 => true // Libraries can always be re-indexed to update new versions
      case 3 => !dataAccessor.hasProgram(ident)
      case _ => false // We only allow G:A or G:A:Vs to be indexed
    }
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

  def getAnalysisRuns(analysisName: String, analysisVersion: String, inputFilter: Option[Long], limit: Int, skip: Int): Try[Set[AnalysisRunRepr]] = {

    if(inputFilter.isDefined){
      dataAccessor
        .getAnalysisRunsForEntity(inputFilter.get, Some(analysisName, analysisVersion), skip, limit)
        .map(allRuns => allRuns.map(toRunRepr))
    } else {
      dataAccessor
        .getAnalysisRuns(analysisName, analysisVersion, includeResults = false, skip, limit)
        .map(allRuns => allRuns.map(toRunRepr))
    }
  }

  def getAnalysisRunsForEntity(entityId: Long, limit: Int, skip: Int): Try[Set[AnalysisRunRepr]] = {
    dataAccessor
      .getAnalysisRunsForEntity(entityId, None, skip, limit)
      .map(allRuns => allRuns.map(toRunRepr))
  }

  def hasAnalysis(analysisName: String, version: Option[String] = None): Boolean = {
    if (version.isDefined) dataAccessor.hasAnalysis(analysisName, version.get)
    else dataAccessor.hasAnalysis(analysisName)
  }

  def hasAnalysisRun(analysisName: String, version: String, runUid: String): Boolean = {
    dataAccessor.hasAnalysisRun(analysisName, version, runUid)
  }

  def getAllEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[Long]): Future[Seq[EntityRepr]] = {

    dataAccessor
      .getEntities(limit, skip, kindFilter, parentFilter)
      .map{ entities =>
        entities.map(toEntityRepr)
      }
  }

  def getEntity(eid: Long, resolutionDepth: Option[Int]): Future[EntityRepr] = {

    // Make sure the default depth is 2. If somebody wants the entire tree, they need to pass a value larger than 5
    dataAccessor
      .getEntity(eid, Some(resolutionDepth.getOrElse(2)))
      .map(toEntityRepr)
  }

  def isLibrary(eid: Long): Boolean = {
    dataAccessor.getEntityKind(eid).map( _ == SoftwareEntityKind.Library).getOrElse(false)
  }

  def getEntityChildren(eid: Long, skip: Int, limit: Int): Try[Seq[EntityRepr]] = Try {
    dataAccessor.getEntityChildren(eid, skip, limit).get.map(toEntityRepr)
  }

  def getAllResultsFor(eid: Long, analysisFilter: Option[String], limit: Int, skip: Int): Try[Set[AnalysisResultRepr]] = {
    val analysisNameAndVersionOpt = analysisFilter.map( s => {
      val parts = s.split(":")
      (parts(0).trim, parts(1).trim)
    })

    dataAccessor
      .getJSONResultsFor(eid, analysisNameAndVersionOpt, limit, skip)
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
        val requestInputsRaw = request.Inputs.toSet
        val requestInputIdOpts = requestInputsRaw.map(iName => dataAccessor.getEntityIdFor(iName.split("!").toIndexedSeq))

        // Run cannot be present if input is not known
        if(requestInputIdOpts.exists(_.isEmpty)) return Success(None)

        Success(
          runs.find{ r =>

            val runInputs = r.inputs.map(_.id)

            r.configuration.equals(request.Configuration) &&
            runInputs.equals(requestInputIdOpts.map(_.get)) &&
            r.state.equals(RunState.Finished)
          }.map(_.uid)
        )
      case Failure(ex) =>
        log.error(s"Failed to retrieve runs from DB for $analysisName:$analysisVersion", ex)
        Failure(ex)
    }
  }

  def validateRunRequest(analysisName: String, analysisVersion: String, request: ExecuteAnalysisRequest): (Boolean, String) = {

    val isIncrementalAnalysis = dataAccessor.isIncrementalAnalysis(analysisName, analysisVersion)

    val allEntityIdOpts = request.Inputs.map(iName => (iName, dataAccessor.getEntityIdFor(iName.split("!").toIndexedSeq)))

    val invalidEntityIds = allEntityIdOpts.filter(_._2.isEmpty).map(_._1)

    if(invalidEntityIds.nonEmpty){
      return (false, s"Input entities not found in index: ${invalidEntityIds.mkString(",")}")
    }

    if(request.BaselineRun.isDefined && !isIncrementalAnalysis) {
      (false, "A baseline run cannot be specified for non-incremental analyses.")
    } else if(!isIncrementalAnalysis){
      (true, "")
    } else if (request.BaselineRun.isEmpty){
      log.warn(s"An incremental analysis has been triggered without specifying a baseline run: $analysisName:$analysisVersion")
      (true, "Running incremental analysis with empty baseline.")
    } else {
      dataAccessor.getAnalysisRun(analysisName, analysisVersion, request.BaselineRun.get) match {
        case Success(_) =>
          (true, "")
        case Failure(_) =>
          (false, s"Invalid baseline run specified, id ${request.BaselineRun.get} not found")
      }
    }
  }

  def triggerNewAnalysisRun(analysisName: String, analysisVersion: String, request: ExecuteAnalysisRequest): Try[String] = Try {

    val isIncrementalAnalysis = dataAccessor.isIncrementalAnalysis(analysisName, analysisVersion)

    // Create new empty record for analysis run
    val newId = dataAccessor.storeEmptyAnalysisRun(analysisName, analysisVersion, request.Configuration).get

    // Queue run execution
    val name = s"$analysisName:$analysisVersion"

    // This "get" works because the validation would have filtered out invalid entity names beforehand
    val entityIds = request.Inputs.toSet[String].map(iName => dataAccessor.getEntityIdFor(iName.split("!").toIndexedSeq).get)

    // We assume all validation has been done beforehand, i.e. validateRunRequest has been called
    val command = AnalysisCommand(name, newId, request.User.getOrElse("Anonymous"), entityIds, request.Configuration)

    val commandJson = if(isIncrementalAnalysis) command.asIncremental(request.BaselineRun).toJson.compactPrint
    else command.toJson.compactPrint

    log.info(s"Publishing a new Runner Command for analysis $name : $commandJson")

    val writer = new MqMessageWriter(configuration.asAnalysisQueuePublishConfig)
    writer.initialize()
    writer.appendToQueue(commandJson)
    writer.shutdown()

    newId

  }

  def identifierToEntityId(identifier: String): Option[Long] = {
    dataAccessor.getEntityIdFor(identifier.split("!").toIndexedSeq)
  }

  def getRunResults(runId: String, limit: Int, skip: Int): Try[Set[AnalysisResultRepr]] = {
    dataAccessor.getRunResultsAsJSON(runId, includeContents = true, skip, limit).map { allResults =>
      allResults.map(result => toResultRepr(result))
    }
  }

  def getAllAnalysisResults(analysisName: String, analysisVersion: String, limit: Int, skip: Int): Future[Set[AnalysisResultRepr]] = {
    dataAccessor.getAllResults(analysisName, analysisVersion, limit, skip).map { allResults => allResults.map(toResultRepr)}
  }

  def triggerEntityMining(entityIdentifiers: Seq[String]): Boolean = {

    Try {
      val writer = new MqMessageWriter(configuration.asMinerQueuePublishConfig)
      writer.initialize()

      val msg = MinerCommand(entityIdentifiers.toSet, None).toJson.compactPrint

      log.info(s"Publishing a new Miner Command for ${entityIdentifiers.size} entities : $msg")

      writer.appendToQueue(msg, Some(3))

      writer.shutdown()
    } match {
      case Success(_) => true
      case Failure(ex) =>
        log.error(s"Failed to enqueue ${entityIdentifiers.size} entity ids.", ex)
        false
    }


  }


}
