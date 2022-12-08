package de.tudo.sse.spareuse.core.storage

import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisRunData}
import spray.json.JsonWriter

import scala.util.Try

trait AnalysisAccessor {

  def initializeAnalysisTables(): Unit

  def getAnalysisData(analysisName: String, analysisVersion: String, includeRuns: Boolean = false): Try[AnalysisData]

  def getAnalysisRunsFor(analysis: AnalysisData, includeResults: Boolean = false): Try[Set[AnalysisRunData]] =
    getAnalysisRuns(analysis.name, analysis.version, includeResults)

  def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean = false): Try[Set[AnalysisRunData]]

  def storeAnalysisRun(run: AnalysisRunData)(implicit serializer: JsonWriter[Object]): Try[Unit]

  def getFreshResultUuids(noOfUuids: Int): Set[String]

  def getFreshRunUuid(): String

  def hasAnalysis(analysisName: String, analysisVersion: String): Boolean = getAnalysisData(analysisName, analysisVersion).isSuccess

  def hasAnalysis(analysisName: String): Boolean

  def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean

  def registerIfNotPresent(analysis: AnalysisData): Unit

}
