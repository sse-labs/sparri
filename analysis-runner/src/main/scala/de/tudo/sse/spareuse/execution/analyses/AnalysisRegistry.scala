package de.tudo.sse.spareuse.execution.analyses

import scala.collection.mutable

object AnalysisRegistry {

  private val analysisLookup:  mutable.Map[String, AnalysisImplementation] = new mutable.HashMap()

  private def combine(str1: String, str2: String) = s"$str1:$str2"

  def registerAnalysisImplementation(analysisImpl: AnalysisImplementation): Unit =
    analysisLookup.put(combine(analysisImpl.name, analysisImpl.version), analysisImpl)

  def analysisImplementationAvailable(analysisName: String, analysisVersion: String): Boolean =
    analysisLookup.contains(combine(analysisName, analysisVersion))

  def getAnalysisImplementation(analysisName: String, analysisVersion: String): AnalysisImplementation =
    analysisLookup(combine(analysisName, analysisVersion))

  def allAnalysisImplementations(): Iterable[AnalysisImplementation] = analysisLookup.values

}
