package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.RunState.RunState
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.DataAccessor
import spray.json.JsonWriter

import java.time.LocalDateTime
import scala.concurrent.Future
import scala.util.{Success, Try}

package object impl {

  def mockDataAccessor_JsonResultsOnly(func: (String, Option[(String, String)]) => Set[AnalysisResultData]): DataAccessor =
    new DataAccessor {
      override def shutdown(): Unit = {}

      override def initializeEntityTables(): Unit = {}
      override def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[String]): Future[Seq[SoftwareEntityData]] = ???
      override def getEntityChildren(uid: String, skip: Int, limit: Int): Try[Seq[SoftwareEntityData]] = ???
      override def getEntityKind(entityIdent: String): Try[SoftwareEntityKind] = ???
      override def getEntity(ident: String, resolutionScope: SoftwareEntityKind): Future[SoftwareEntityData] = ???
      override def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean = ???
      override def hasEntity(ident: String): Boolean = ???
      override def initializeAnalysisTables(): Unit = {}
      override def getAnalyses(includeRuns: Boolean, skip: Int, limit: Int): Try[Set[AnalysisData]] = ???
      override def getAnalysesFor(analysisName: String, includeRuns: Boolean): Try[Set[AnalysisData]] = ???
      override def getAnalysisData(analysisName: String, analysisVersion: String, includeRuns: Boolean): Try[AnalysisData] = ???
      override def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean, skip: Int, limit: Int): Try[Set[AnalysisRunData]] = ???
      override def getAnalysisRunsForEntity(entityName: String, skip: Int, limit: Int): Try[Set[AnalysisRunData]] = ???
      override def getAnalysisRun(analysisName: String, analysisVersion: String, runUid: String, includeResults: Boolean): Try[AnalysisRunData] = ???
      override def storeEmptyAnalysisRun(analysisName: String, analysisVersion: String, runConfig: String): Try[String] = ???
      override def setRunResults(runUid: String, timeStamp: LocalDateTime, logs: Array[String], freshResults: Set[AnalysisResultData], unchangedResultIds: Set[String])(implicit serializer: JsonWriter[Object]): Try[Unit] = ???
      override def getRunResultsAsJSON(runUid: String, skip: Int, limit: Int): Try[Set[AnalysisResultData]] = ???
      override def setRunState(runUid: String, state: RunState, runInputIdsOpt: Option[Set[String]]): Try[Unit] = ???
      override def getFreshResultUuids(noOfUuids: Int): Set[String] = ???
      override def getFreshRunUuid: String = ???
      override def hasAnalysis(analysisName: String): Boolean = ???
      override def isIncrementalAnalysis(analysisName: String, analysisVersion: String): Boolean = ???
      override def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean = ???
      override def getJSONResultsFor(entityName: String, analysisFilter: Option[(String, String)], limit: Int, skip: Int): Try[Set[AnalysisResultData]] =
        Success(func(entityName, analysisFilter))
      override def getAllResults(analysisName: String, analysisVersion: String, limit: Int, skip: Int): Future[Set[AnalysisResultData]] = ???
      override def registerIfNotPresent(analysis: AnalysisData): Unit = ???
    }

}
