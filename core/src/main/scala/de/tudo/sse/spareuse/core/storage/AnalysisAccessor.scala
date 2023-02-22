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

  /**
   * This Method retrieves all results for the given run, but does not deserialize the contents into an actual object
   * structure. This is meant for all use-cases in the API, where results will be retrieved, serialized and returned
   * to the user either way, thus saving a deserialization-serialization roundtrip.
   * @param runUid UUID of the analysis run to return results for
   * @param skip Number of result entries to skip. Default is 0.
   * @param limit Number of result entries to retrieve. Default is 100.
   * @return Try of the set of results. The 'content: Object' attribute will hold the result contents as a PLAIN JSON STRING
   */
  def getRunResultsAsJSON(runUid: String, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisResultData]]

  def setRunState(runUid: String, state: RunState): Try[Unit]

  def getFreshResultUuids(noOfUuids: Int): Set[String]

  def getFreshRunUuid(): String

  def hasAnalysis(analysisName: String, analysisVersion: String): Boolean = getAnalysisData(analysisName, analysisVersion).isSuccess

  def hasAnalysis(analysisName: String): Boolean

  def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean

  def getResultsFor(entityName: String, analysisFilter: Option[(String, String)], limit: Int, skip: Int): Try[Set[AnalysisResultData]]

  def registerIfNotPresent(analysis: AnalysisData): Unit

}
