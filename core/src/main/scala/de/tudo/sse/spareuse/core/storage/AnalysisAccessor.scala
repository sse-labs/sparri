package de.tudo.sse.spareuse.core.storage

import de.tudo.sse.spareuse.core.model.RunState.RunState
import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import spray.json.JsonWriter

import java.time.LocalDateTime
import scala.util.Try

trait AnalysisAccessor {

  def initializeAnalysisTables(): Unit

  def getAnalysisData(analysisName: String, analysisVersion: String, includeRuns: Boolean = false): Try[AnalysisData]

  def getAnalysisRunsFor(analysis: AnalysisData, includeResults: Boolean = false): Try[Set[AnalysisRunData]] =
    getAnalysisRuns(analysis.name, analysis.version, includeResults)

  def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean = false): Try[Set[AnalysisRunData]]

  def getAnalysisRun(analysisName: String, analysisVersion: String, runUid: String, includeResults: Boolean = false): Try[AnalysisRunData]

  def storeEmptyAnalysisRun(analysisName: String, analysisVersion: String, runConfig: String, runInputIds: Set[String]): Try[String]

  def setRunResults(runUid: String, timeStamp: LocalDateTime, logs: Array[String], results: Set[AnalysisResultData])(implicit serializer: JsonWriter[Object]): Try[Unit]

  def setRunState(runUid: String, state: RunState): Try[Unit]

  def getFreshResultUuids(noOfUuids: Int): Set[String]

  def getFreshRunUuid(): String

  def hasAnalysis(analysisName: String, analysisVersion: String): Boolean = getAnalysisData(analysisName, analysisVersion).isSuccess

  def hasAnalysis(analysisName: String): Boolean

  def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean

  def getResultsFor(entityName: String, analysisFilter: Option[(String, String)], limit: Int, skip: Int): Try[Set[AnalysisResultData]]

  def registerIfNotPresent(analysis: AnalysisData): Unit

}
