package de.tudo.sse.spareuse.execution.analyses

import scala.collection.mutable

object AnalysisRegistry {

  private val analysisLookup:  mutable.Map[String, AnalysisImplementation] = new mutable.HashMap()

  def registerAnalysisImplementation(analysisImpl: AnalysisImplementation): Unit = analysisLookup.put(analysisImpl.name, analysisImpl)

  def analysisImplementationAvailable(analysisName: String): Boolean = analysisLookup.contains(analysisName)

  def getAnalysisImplementation(analysisName: String): AnalysisImplementation = analysisLookup(analysisName)

}
