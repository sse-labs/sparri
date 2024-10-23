package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.formats.{AnalysisResultFormat, AnyValueFormat, BaseValueFormat, EmptyFormat, EntityReferenceFormat, GraphResultFormat, ListResultFormat, MapResultFormat, NamedPropertyFormat, NumberFormat, ObjectResultFormat, StringFormat}
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, RunState}
import org.anon.spareuse.core.model.RunState.RunState
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.utils.ObjectCache
import slick.dbio.DBIO
import slick.jdbc.PostgresProfile.api._
import spray.json.JsonWriter

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait PostgresAnalysisAccessor {
  this: PostgresEntityAccessor with PostgresSparriSupport =>

  implicit val executor: ExecutionContext

  private final val analysisResultCache: ObjectCache[Long, AnalysisResultData] = new ObjectCache[Long, AnalysisResultData](100000)

  override def registerIfNotPresent(analysis: AnalysisData): Unit = {
    if (!hasAnalysis(analysis.name, analysis.version)) {

      val formatId = storeResultFormat(analysis.resultFormat)

      val insertF = db.run(analysesTable += toAnalysisRepr(analysis, formatId))

      Await.result(insertF, simpleQueryTimeout)
    }
  }

  private def getAnalysisRepr(analysisName: String, analysisVersion: String): SoftwareAnalysisRepr = {
    val queryF = db.run(analysesTable.filter(a => a.name === analysisName && a.version === analysisVersion).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(result) => result
      case None => throw new IllegalStateException(s"Analysis $analysisName:$analysisVersion no present in db")
    }
  }

  override def getAnalysisData(analysisName: String, analysisVersion: String, includeRuns: Boolean = false): Try[AnalysisData] = Try {

    val repr = getAnalysisRepr(analysisName, analysisVersion)

    val analysisRuns = if (includeRuns) getAnalysisRuns(repr.id, analysisName, analysisVersion, includeResults = false, skip = 0, limit = 500)
    else Set.empty[AnalysisRunData]

    getResultFormat(repr.formatId) match {
      case f: AnalysisResultFormat =>
        repr.toAnalysisData(f, analysisRuns)
      case _ =>
        throw new IllegalStateException("Corrupt format definition: Analysis result format must not be base value")
    }
  }


  override def getAnalyses(includeRuns: Boolean = false, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisData]] = Try {

    val queryF = db.run(analysesTable.sortBy(_.id).drop(skip).take(limit).result)

    Await
      .result(queryF, simpleQueryTimeout)
      .map { analysisRepr =>
        val analysisRuns = if (includeRuns) getAnalysisRuns(analysisRepr.id, analysisRepr.name, analysisRepr.version, includeResults = false, skip = 0, limit = 500)
        else Set.empty[AnalysisRunData]

        getResultFormat(analysisRepr.formatId) match {
          case f: AnalysisResultFormat =>
            analysisRepr.toAnalysisData(f, analysisRuns)
          case _ =>
            throw new IllegalStateException("Corrupt format definition: Analysis result format must not be base value")
        }
      }.toSet
  }

  override def getAnalysesFor(analysisName: String, includeRuns: Boolean = false): Try[Set[AnalysisData]] = Try {
    val queryF = db.run(analysesTable.filter(_.name === analysisName).result)

    Await
      .result(queryF, simpleQueryTimeout)
      .map { analysisRepr =>
        val analysisRuns = if (includeRuns) getAnalysisRuns(analysisRepr.id, analysisRepr.name, analysisRepr.version, includeResults = false, skip = 0, limit = 500)
        else Set.empty[AnalysisRunData]

        getResultFormat(analysisRepr.formatId) match {
          case f: AnalysisResultFormat =>
            analysisRepr.toAnalysisData(f, analysisRuns)
          case _ =>
            throw new IllegalStateException("Corrupt format definition: Analysis result format must not be base value")
        }
      }.toSet
  }

  private def getInputsForRun(analysisRunId: Long): Set[SoftwareEntityData] = {
    val queryF = db.run {
      val runIdToInput = for {(_, i) <- analysisRunInputsTable.filter(t => t.analysisRunID === analysisRunId) join entitiesTable on (_.inputEntityID === _.id)} yield i

      runIdToInput.result
    }
      .map { entityReprs => buildEntities(entityReprs) }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  private def getAnalysisRuns(analysisId: Long, analysisName: String, analysisVersion: String, includeResults: Boolean, skip: Int, limit: Int): Set[AnalysisRunData] = {

    val queryF = db
      .run(analysisRunsTable.filter(r => r.parentID === analysisId).sortBy(_.id).drop(skip).take(limit).result)
      .map { allRuns =>
        allRuns.map { run =>
          val results = if (includeResults) {
            getRunResultsAsJSON(run.uid, includeContents = true).get
          } else Set.empty[AnalysisResultData]

          run.toAnalysisRunData(analysisName, analysisVersion, getInputsForRun(run.id), results)
        }
      }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  override def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisRunData]] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    getAnalysisRuns(analysisId, analysisName, analysisVersion, includeResults, skip, limit)
  }

  override def getAnalysisRunsForEntity(eid: Long, analysisFilter: Option[(String, String)], skip: Int, limit: Int): Try[Set[AnalysisRunData]] = Try {

    val allRunsQuery = for (((_, run), analysis) <- analysisRunInputsTable.filter(_.inputEntityID === eid) join analysisRunsTable on (_.analysisRunID === _.id) join analysesTable on (_._2.parentID === _.id))
      yield (run, analysis)

    val theQuery = if (analysisFilter.isDefined) {
      val analysisRepr = getAnalysisRepr(analysisFilter.get._1, analysisFilter.get._2)

      allRunsQuery filter (_._2.id === analysisRepr.id) drop skip take limit
    } else {
      allRunsQuery drop skip take limit
    }

    val resultF = db.run(theQuery.result)
      .map { allResults =>
        allResults.map {
          case (run, analysis) =>
            run.toAnalysisRunData(analysis.name, analysis.version, getInputsForRun(run.id), Set.empty[AnalysisResultData])
        }.toSet
      }(db.ioExecutionContext)


    Await.result(resultF, longActionTimeout)
  }

  override def getAnalysisRun(analysisName: String, analysisVersion: String, runUid: String, includeResults: Boolean = false, includeResultContents: Boolean = false): Try[AnalysisRunData] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    def getAllRunResults: Set[AnalysisResultData] = {

      val take = 100

      var theResults = getRunResultsAsJSON(runUid, includeResultContents, 0, take).get
      var roundResultsCnt = theResults.size
      var round = 1

      while(roundResultsCnt == take){
        val roundResults = getRunResultsAsJSON(runUid, includeResultContents, round * take, take).get
        theResults = theResults ++ roundResults
        roundResultsCnt = roundResults.size
        round = round + 1
      }

      theResults
    }

    val results = if (includeResults) {
      getAllRunResults
    } else Set.empty[AnalysisResultData]

    val queryF = db
      .run(analysisRunsTable.filter(r => r.parentID === analysisId && r.uid === runUid).take(1).result)
      .map(r => r.map(run => run.toAnalysisRunData(analysisName, analysisVersion, getInputsForRun(run.id), results)))(db.ioExecutionContext)

    Await.result(queryF, simpleQueryTimeout).head
  }

  def getNoOfFreshAndTotalResults(runUid: String): Try[(Int, Int)] = Try {
    val runId = getRunRepr(runUid).id
    val allResultIds = Await.result(db.run(runResultsTable.filter(_.analysisRunID === runId).map(_.resultID).result), longActionTimeout)

    val allFreshResultIds = Await.result(db.run(analysisResultsTable.filter(_.runID === runId).map(_.id).result), longActionTimeout)

    (allFreshResultIds.size, allResultIds.size)
  }

  private def getRunRepr(runUid: String): SoftwareAnalysisRunRepr = {
    val queryF = db.run(analysisRunsTable.filter(r => r.uid === runUid).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(result) => result
      case None => throw new IllegalStateException(s"Analysis Run with uid $runUid not present in db")
    }

  }

  private def setRunState(runId: Long, runState: Int): Unit = {
    val action = db.run(analysisRunsTable.filter(r => r.id === runId).map(r => r.state).update(runState))
    Await.ready(action, simpleQueryTimeout)
  }

  override def setRunState(runUid: String, state: RunState, runInputIdsOpt: Option[Set[Long]]): Try[Unit] = Try {
    val runRepr = getRunRepr(runUid)
    setRunState(runRepr.id, state.id)

    if (runInputIdsOpt.isDefined) {
      // Connect inputs to run
      val inputEntityIds = Await.result(db.run(entitiesTable.filter(e => e.id inSet runInputIdsOpt.get).map(_.id).result), simpleQueryTimeout)

      val inputInsertAction = db.run(DBIO.sequence(inputEntityIds.map(id => analysisRunInputsTable += AnalysisRunInput(-1, runRepr.id, id))))
      Await.ready(inputInsertAction, simpleQueryTimeout)
    }
  }

  override def setRunResults(runUid: String,
                             timeStamp: LocalDateTime,
                             durationMs: Long,
                             logs: Array[String],
                             freshResults: Set[AnalysisResultData],
                             unchangedResultIds: Set[String])(implicit serializer: JsonWriter[Object]): Try[Unit] = Try {

    val runRepr: SoftwareAnalysisRunRepr = getRunRepr(runUid)
    setRunState(runRepr.id, RunState.Finished.id)

    val newTimeStamp = timeStamp.format(DateTimeFormatter.ISO_DATE_TIME)
    val logsString = logs.mkString(";;;")

    val updateTimeStampAction = db.run(analysisRunsTable.filter(r => r.uid === runUid).map(r => r.timestamp).update(newTimeStamp))
    val updateDurationAction = db.run(analysisRunsTable.filter(r => r.uid === runUid).map(_.duration).update(durationMs))
    val updateLogsAction = db.run(analysisRunsTable.filter(r => r.uid === runUid).map(r => r.logs).update(logsString))

    Await.ready(updateTimeStampAction, simpleQueryTimeout)
    Await.ready(updateDurationAction, simpleQueryTimeout)
    Await.ready(updateLogsAction, simpleQueryTimeout)

    val resultStorageFuture = Future.sequence(freshResults
      .grouped(100)
      .map { runResultBatch =>
        Future {
          runResultBatch.map { runResult =>
            val content = serializer.write(runResult.content).compactPrint
            AnalysisResult(-1, runResult.uid, runRepr.id, runResult.isRevoked, content)
          }

        }.flatMap{ resultObjBatch =>
          db.run(idAndUidReturningResultTable ++= resultObjBatch).map(resultSeq => resultSeq.map(tuple => (tuple._2, tuple._1)))
        }

      })

    val freshResultUidToIdMap = Await.result(resultStorageFuture, 10.minutes).flatten.toMap

    val allValidities = freshResults.flatMap{ fResult =>
      val dbId = freshResultUidToIdMap(fResult.uid)
      analysisResultCache.pushValue(dbId, fResult)
      fResult.affectedEntities.map { affectedEntity => AnalysisResultValidity(-1, dbId, affectedEntity.id) }
    }

    val allValiditiesFuture = Future.sequence(allValidities.grouped(100).map{ validitiesBatch =>
      db.run(resultValiditiesTable ++= validitiesBatch)
    })

    Await.ready(allValiditiesFuture, longActionTimeout)

    val resultDbIdsAction = db.run(analysisResultsTable.filter(r => r.uid inSet unchangedResultIds).map(_.id).result)
    val unchangedResultIdsInDb = Await.result(resultDbIdsAction, simpleQueryTimeout)

    val resultRelationFuture = Future.sequence((freshResultUidToIdMap.values ++ unchangedResultIdsInDb)
      .map(id => AnalysisRunResultRelation(-1, runRepr.id, id))
      .grouped(100)
      .map { resultRelationBatch =>
        db.run(runResultsTable ++= resultRelationBatch)
      })

    Await.ready(resultRelationFuture, longActionTimeout)

  }

  override def getRunResultsAsJSON(runUid: String, includeContents: Boolean, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisResultData]] = Try {

    val runRepr: SoftwareAnalysisRunRepr = getRunRepr(runUid)

    val lookupF = db
      .run(runResultsTable.filter(rr => rr.analysisRunID === runRepr.id).sortBy(_.id).drop(skip).take(limit).map(_.resultID).result)
      .flatMap { allResultIds =>
        val cached = allResultIds.map(analysisResultCache.getValueOpt).filter(_.isDefined).map(_.get)
        val notCachedIds = allResultIds.filterNot(analysisResultCache.hasValue)

        val query = analysisResultsTable.filter(_.id inSet notCachedIds)
        val resultsQuery = if (!includeContents) query.map(t => (t.id, t.uid, t.runID, t.isRevoked, ""))
        else query.map(t => (t.id, t.uid, t.runID, t.isRevoked, t.content))

        db.run(resultsQuery.result).flatMap { tuples =>
          val actualResults = tuples
            .map { case (id, uid, runID, isRevoked, content) => AnalysisResult(id, uid, runID, isRevoked, content) }

          db.run {
            val join = for {(resultValidity, entity) <- resultValiditiesTable.filter(_.resultId inSet notCachedIds) join entitiesTable on (_.entityId === _.id)}
              yield (resultValidity.resultId, entity)
            join.result
          }.map { mappingData =>
            val resultToInputEntitiesMapping = mappingData.groupMap(t => t._1)(t => toGenericEntityData(t._2))

            val freshResultData = actualResults.map { resultRep =>
              val allAssociatedEntities = resultToInputEntitiesMapping(resultRep.id)

              val runIdQuery = db.run(analysisRunsTable.filter(_.id === resultRep.runId).map(_.uid).result)
              val producingRunUid = Await.result(runIdQuery, simpleQueryTimeout).head

              (resultRep.id, AnalysisResultData(resultRep.uid, resultRep.isRevoked, producingRunUid, resultRep.jsonContent, allAssociatedEntities.toSet))
            }.toSet

            freshResultData.foreach(t => analysisResultCache.pushValue(t._1, t._2))

            freshResultData.map(_._2) ++ cached

          }
        }


      }

    Await.result(lookupF, 20.minutes)
  }


  override def storeEmptyAnalysisRun(analysisName: String, analysisVersion: String, runConfig: String): Try[String] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    // Insert run
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME)
    val runRepr = SoftwareAnalysisRunRepr(-1, getFreshRunUuid, runConfig, RunState.Created.id, isRevoked = false, analysisId, "", timestamp, 0L)
    Await.ready(db.run(idReturningAnalysisRunTable += runRepr), simpleQueryTimeout)

    runRepr.uid
  }

  override def getFreshResultUuids(noOfUuids: Int): Set[String] = {

    var uuids = Range(0, noOfUuids).map(_ => UUID.randomUUID().toString).toSet

    def newUuids(): Unit = uuids = Range(0, noOfUuids).map(_ => UUID.randomUUID().toString).toSet

    val query = analysisResultsTable.filter(r => r.uid inSet uuids).exists

    var uuidsFoundInDB = Await.result(db.run(query.result), simpleQueryTimeout)

    while (uuids.size < noOfUuids || uuidsFoundInDB) {
      newUuids()
      uuidsFoundInDB = Await.result(db.run(query.result), simpleQueryTimeout)
    }

    uuids
  }

  override def getFreshRunUuid: String = {
    var uuid = UUID.randomUUID().toString

    def exists: Boolean = Await.result(db.run(analysisRunsTable.filter(r => r.uid === uuid).exists.result), simpleQueryTimeout)

    while (exists) {
      uuid = UUID.randomUUID().toString
    }

    uuid
  }

  override def getJSONResultsFor(eid: Long, analysisFilter: Option[(String, String)], limit: Int, skip: Int): Try[Set[AnalysisResultData]] = Try {

    // Get all results associated with this entity
    val allEntityResultsQuery =
      for {(_, result) <- resultValiditiesTable.filter(v => v.entityId === eid) join analysisResultsTable on (_.resultId === _.id)}
        yield result
    //join.sortBy(_.id).drop(skip).take(limit).result


    val allEntityResults = {
      if (analysisFilter.isDefined) {
        val analysisRepr = getAnalysisRepr(analysisFilter.get._1, analysisFilter.get._2)

        val filteredResultsQuery = for {(result, _) <- allEntityResultsQuery join analysisRunsTable on (_.runID === _.id) filter (_._2.parentID === analysisRepr.id)}
          yield result

        Await.result(db.run(filteredResultsQuery.drop(skip).take(limit).result), longActionTimeout)
      } else {
        Await.result(db.run(allEntityResultsQuery.drop(skip).take(limit).result), longActionTimeout)
      }
    }

    // Results may be associated with more than one entity, therefore: Collect mapping of results to entities for all results
    val resultEntitiesF = db.run {
      val join = for {(resultValidity, entity) <- resultValiditiesTable.filter(v => v.resultId inSet allEntityResults.map(_.id)) join entitiesTable on (_.entityId === _.id)}
        yield (resultValidity.resultId, entity)
      join.result
    }

    val resultToInputEntitiesMapping = Await.result(resultEntitiesF, longActionTimeout)

    // Build results
    allEntityResults.map { resultRep =>
      val allAssociatedEntities = resultToInputEntitiesMapping
        .filter(t => t._1 == resultRep.id)
        .map(t => toGenericEntityData(t._2))

      val runIdQuery = db.run(analysisRunsTable.filter(_.id === resultRep.runId).map(_.uid).result)
      val producingRunUid = Await.result(runIdQuery, simpleQueryTimeout).head

      AnalysisResultData(resultRep.uid, resultRep.isRevoked, producingRunUid, resultRep.jsonContent, allAssociatedEntities.toSet)
    }.toSet

  }

  def getAllResults(analysisName: String, analysisVersion: String, limit: Int, skip: Int): Future[Set[AnalysisResultData]] = {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    val resultIdQuery = for {(_, runResultRelation) <- analysisRunsTable.filter(_.parentID === analysisId) join runResultsTable on (_.id === _.analysisRunID) }
      yield runResultRelation.resultID

    db.run(resultIdQuery.sorted.drop(skip).take(limit).result)
      .flatMap{ resultIds =>
        db.run(analysisResultsTable.filter(r => r.id inSet resultIds).result)
      }
      .flatMap{ resultsData =>
        val resultEntitiesF = db.run {
          val join = for {(resultValidity, entity) <- resultValiditiesTable.filter(v => v.resultId inSet resultsData.map(_.id)) join entitiesTable on (_.entityId === _.id)}
            yield (resultValidity.resultId, entity)
          join.result
        }

        resultEntitiesF.map(resultToEntitiesMap => (resultsData, resultToEntitiesMap))
      }
      .flatMap{ case (resultsData, resultToEntitiesMapping) =>
        val runUidMappingF = db.run(analysisRunsTable.filter(_.id inSet resultsData.map(_.runId)).map(r => (r.id, r.uid)).result)

        runUidMappingF.map(m => (resultsData, resultToEntitiesMapping, m.toMap))
      }
      .map{ case (resultsData, resultToEntitiesMapping, runToUidMapping) =>
        resultsData
          .map{ data =>
            AnalysisResultData(data.uid, data.isRevoked, runToUidMapping(data.runId), data.jsonContent, resultToEntitiesMapping.filter(_._1 == data.id).map(_._2).map(toGenericEntityData).toSet)
          }
          .toSet
      }


  }

  implicit private def tryToBool: Try[Boolean] => Boolean = {
    case Success(value) => value
    case Failure(ex) =>
      log.error("Failed to perform DB lookup", ex)
      false

  }

  override def hasAnalysis(analysisName: String): Boolean = Try {
    val queryF = db.run(analysesTable.filter(a => a.name === analysisName).exists.result)
    Await.result(queryF, simpleQueryTimeout)
  }

  override def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean = Try {
    val requiredId = getAnalysisRepr(analysisName, analysisVersion).id
    val queryF = db.run(analysisRunsTable.filter(r => r.uid === runUid && r.parentID === requiredId).exists.result)

    Await.result(queryF, simpleQueryTimeout)
  }


  override def isIncrementalAnalysis(analysisName: String, analysisVersion: String): Boolean = Try {
    val queryF = db.run(analysesTable.filter(a => a.name === analysisName && a.version === analysisVersion).map(_.isIncremental).result)

    val queryResult = Await.result(queryF, simpleQueryTimeout)

    queryResult.nonEmpty && queryResult.head
  }

  private[storage] def storeResultFormat(format: AnyValueFormat): Long = {
    if (format.isBaseValue) {
      format match {
        case StringFormat => ResultFormatPredef.StringFormat.id
        case NumberFormat => ResultFormatPredef.NumberFormat.id
        case EntityReferenceFormat => ResultFormatPredef.EntityRefFormat.id
        case EmptyFormat => ResultFormatPredef.EmptyFormat.id
      }
    } else {

      format match {
        case ListResultFormat(elemF, explanation) =>
          val elementFormatId = storeResultFormat(elemF)

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_LIST", ResultType.ListResult.id)), simpleQueryTimeout)

          Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, elementFormatId, ResultNestingKind.ListElement.id, explanation)), simpleQueryTimeout)

          formatId

        case MapResultFormat(keyF, valueF, keyExplanation, valueExplanation) =>
          val keyFormatId = storeResultFormat(keyF)
          val valueFormatId = storeResultFormat(valueF)

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_MAP", ResultType.MapResult.id)), simpleQueryTimeout)

          val keyAndValueInsert = DBIO.seq(nestedResultFormatsTable += NestedResultFormatReference(formatId, keyFormatId, ResultNestingKind.MapKey.id, keyExplanation),
            nestedResultFormatsTable += NestedResultFormatReference(formatId, valueFormatId, ResultNestingKind.MapValue.id, valueExplanation))

          Await.ready(db.run(keyAndValueInsert), simpleQueryTimeout)

          formatId

        case ObjectResultFormat(propFormats) =>

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_OBJECT", ResultType.ObjectResult.id)), simpleQueryTimeout)

          propFormats.foreach { propFormat =>

            val propFormatId = storeResultFormat(propFormat)

            Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, propFormatId, ResultNestingKind.ObjectProperty.id, "")), simpleQueryTimeout)
          }

          formatId

        case GraphResultFormat(nodeFormats, edgeFormats, nodeDescription, edgeDescription) =>

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_GRAPH", ResultType.GraphResult.id)), simpleQueryTimeout)

          nodeFormats.foreach { nodeProp =>
            val propFormatId = storeResultFormat(nodeProp)

            Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, propFormatId, ResultNestingKind.NodeProperty.id, nodeDescription)), simpleQueryTimeout)
          }

          edgeFormats.foreach { edgeProp =>
            val propFormatId = storeResultFormat(edgeProp)

            Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, propFormatId, ResultNestingKind.EdgeProperty.id, edgeDescription)), simpleQueryTimeout)
          }

          formatId

        case NamedPropertyFormat(propertyName, propertyFormat, explanation) =>
          val innerFormatId = storeResultFormat(propertyFormat)

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, propertyName, ResultType.NamedPropertyResult.id)), simpleQueryTimeout)

          Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, innerFormatId, ResultNestingKind.NamedProperty.id, explanation)), simpleQueryTimeout)

          formatId
      }
    }

  }

  /**
   * This method deletes an analysis run and all associated results.
   *
   * ATTENTION: This method also deletes results that are referred to by other runs, making other runs' results incomplete.
   * This really only works if all runs of that analysis are deleted, anyway. SHOULD ONLY BE USED FOR DEVELOPMENT PURPOSES!
   *
   * @param runUid runUID to delete
   * @return /
   */
  def removeAnalysisRun(runUid: String): Try[Unit] = Try {
    val runRepr = getRunRepr(runUid)

    val allRunResultIds = Await.result(db.run(runResultsTable.filter(_.analysisRunID === runRepr.id).map(_.resultID).result), longActionTimeout)
    // Delete connection from runs to results
    Await.ready(db.run(runResultsTable.filter(_.analysisRunID === runRepr.id).delete), longActionTimeout)
    // Delete connection from runs to inputs
    Await.ready(db.run(analysisRunInputsTable.filter(_.analysisRunID === runRepr.id).delete), longActionTimeout)
    // Remove run
    allRunResultIds.foreach { runResultId =>
      Await.ready(db.run(resultValiditiesTable.filter(_.resultId === runResultId).delete), longActionTimeout)
      Await.ready(db.run(analysisResultsTable.filter(_.id === runResultId).delete), simpleQueryTimeout)
    }

    // Delete actual run entry
    Await.ready(db.run(analysisRunsTable.filter(_.id === runRepr.id).delete), simpleQueryTimeout)
  }

  private[storage] def getResultFormat(rootId: Long): AnyValueFormat = {

    def getNestedFormats: Seq[NestedResultFormatReference] = {
      val queryF = db.run(nestedResultFormatsTable.filter(n => n.originId === rootId).result)
      Await.result(queryF, simpleQueryTimeout)
    }

    rootId match {
      // Don't need to read from DB for base values
      case ResultFormatPredef.StringFormat.id => StringFormat
      case ResultFormatPredef.NumberFormat.id => NumberFormat
      case ResultFormatPredef.EntityRefFormat.id => EntityReferenceFormat
      case ResultFormatPredef.EmptyFormat.id => EmptyFormat

      // All other ids will be some form of composite format
      case _ =>
        // Get raw format entry from DB, get list of all nested formats
        val rawFormatRepr = Await.result(db.run(resultFormatsTable.filter(f => f.id === rootId).take(1).result), simpleQueryTimeout).head
        val nestedFormats = getNestedFormats

        // Parse nested formats based on format type
        rawFormatRepr.resultType match {
          case ResultType.listTypeId if nestedFormats.size == 1 =>

            val elemFormatNesting = nestedFormats.head
            val elemFormat = getResultFormat(elemFormatNesting.targetId)
            ListResultFormat(elemFormat, elemFormatNesting.description)

          case ResultType.mapTypeId if nestedFormats.size == 2 =>

            val keyFormatNesting = nestedFormats.find(n => n.nestingKind == ResultNestingKind.MapKey.id).get
            val valueFormatNesting = nestedFormats.find(n => n.nestingKind == ResultNestingKind.MapValue.id).get

            val keyFormat = getResultFormat(keyFormatNesting.targetId)
            val valueFormat = getResultFormat(valueFormatNesting.targetId)

            if (!keyFormat.isBaseValue)
              throw new IllegalStateException("Corrupt format definition: Map key needs to be base value")

            MapResultFormat(keyFormat.asInstanceOf[BaseValueFormat], valueFormat, keyFormatNesting.description, valueFormatNesting.description)

          case ResultType.objectTypeId =>

            val objectProperties = nestedFormats.map { nesting =>
              val propFormat = getResultFormat(nesting.targetId)

              if (!propFormat.isInstanceOf[NamedPropertyFormat])
                throw new IllegalStateException("Corrupt format definition: Nested formats in object need to be Named Properties")

              propFormat.asInstanceOf[NamedPropertyFormat]
            }.toSet

            ObjectResultFormat(objectProperties)

          case ResultType.graphTypeId =>

            val nodePropNestings = nestedFormats.filter(n => n.nestingKind == ResultNestingKind.NodeProperty.id)
            val edgePropNestings = nestedFormats.filter(n => n.nestingKind == ResultNestingKind.EdgeProperty.id)

            var nodeDescription = ""

            val nodeProps = nodePropNestings.map { nesting =>
              if (nodeDescription.equals("")) nodeDescription = nesting.description
              val format = getResultFormat(nesting.targetId)


              if (!format.isInstanceOf[NamedPropertyFormat])
                throw new IllegalStateException("Corrupt format definition: Nested formats in graph nodes need to be Named Properties")

              format.asInstanceOf[NamedPropertyFormat]
            }.toSet

            var edgeDescription = ""

            val edgeProps = edgePropNestings.map { nesting =>
              if (edgeDescription.equals("")) edgeDescription = nesting.description
              val format = getResultFormat(nesting.targetId)

              if (!format.isInstanceOf[NamedPropertyFormat])
                throw new IllegalStateException("Corrupt format definition: Nested formats in graph edges need to be Named Properties")

              format.asInstanceOf[NamedPropertyFormat]
            }.toSet

            GraphResultFormat(edgeProps, nodeProps, nodeDescription, edgeDescription)

          case ResultType.namedPropTypeId if nestedFormats.size == 1 =>

            val innerFormatNesting = nestedFormats.head

            val innerFormat = getResultFormat(innerFormatNesting.targetId)

            NamedPropertyFormat(rawFormatRepr.identifier, innerFormat, innerFormatNesting.description)
        }


    }

  }
}
