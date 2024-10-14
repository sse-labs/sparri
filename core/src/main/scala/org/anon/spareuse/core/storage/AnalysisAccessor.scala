package org.anon.spareuse.core.storage

import org.anon.spareuse.core.model.RunState.RunState
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import spray.json.JsonWriter

import java.time.LocalDateTime
import scala.concurrent.Future
import scala.util.Try

trait AnalysisAccessor {

  def initializeAnalysisTables(): Unit

  def getAnalyses(includeRuns: Boolean = false, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisData]]

  def getAnalysesFor(analysisName: String, includeRuns: Boolean = false): Try[Set[AnalysisData]]

  def getAnalysisData(analysisName: String, analysisVersion: String, includeRuns: Boolean = false): Try[AnalysisData]

  def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean = false, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisRunData]]

  def getAnalysisRunsForEntity(eid: Long, analysisFilter: Option[(String, String)], skip: Int = 0, limit: Int = 100): Try[Set[AnalysisRunData]]

  def getAnalysisRun(analysisName: String, analysisVersion: String, runUid: String, includeResults: Boolean = false): Try[AnalysisRunData]

  def storeEmptyAnalysisRun(analysisName: String, analysisVersion: String, runConfig: String): Try[String]

  /**
   * Sets the results for a given run, as well as its timestamp and logs. Will set the run's state to 'Finished'.
   * @param runUid UID of the run to set results for
   * @param timeStamp Timestamp to associate with the given run
   * @param durationMs Duration of the analysis run in millis
   * @param logs Logs to associated with the given run
   * @param freshResults Results freshly introduced with this run, i.e. not existing in the DB so far
   * @param unchangedResultIds IDs of results that already exist in the DB, and are also valid (without changes) for the given run
   * @param serializer Serializer for results produced by this run
   * @return Unit if successful, Failure otherwise
   */
  def setRunResults(runUid: String,
                    timeStamp: LocalDateTime,
                    durationMs: Long,
                    logs: Array[String],
                    freshResults: Set[AnalysisResultData],
                    unchangedResultIds: Set[String])(implicit serializer: JsonWriter[Object]): Try[Unit]

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

  def setRunState(runUid: String, state: RunState, runInputIdsOpt: Option[Set[Long]]): Try[Unit]

  def getFreshResultUuids(noOfUuids: Int): Set[String]

  def getFreshRunUuid: String

  def hasAnalysis(analysisName: String, analysisVersion: String): Boolean = getAnalysisData(analysisName, analysisVersion).isSuccess

  def hasAnalysis(analysisName: String): Boolean

  def isIncrementalAnalysis(analysisName: String, analysisVersion: String): Boolean

  def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean

  /**
   * This methods retrieves all results associated with the given entity, optionally filtered by analysis name and version.
   * Result contents are NOT deserialized, but retrieved as their JSON String representation. This is meant for all
   * use-cases in the API, where results will be retrieved, serialized and returned to the user either way, thus saving a
   * deserialization-serialization roundtrip.
   *
   * @param eid Unique entity ID to retrieve results for
   * @param analysisFilter Optional filter for a given pair of analysis name and version
   * @param limit pagination: Number of results to retrieve
   * @param skip pagination: Number of results to skip
   * @return Try of the set of results for this entity. The 'content: Object' attribute will hold the result contents as
   *         a PLAIN JSON STRING
   */
  def getJSONResultsFor(eid: Long, analysisFilter: Option[(String, String)], limit: Int, skip: Int): Try[Set[AnalysisResultData]]

  def getAllResults(analysisName: String, analysisVersion: String, limit: Int, skip: Int): Future[Set[AnalysisResultData]]

  def registerIfNotPresent(analysis: AnalysisData): Unit

}
