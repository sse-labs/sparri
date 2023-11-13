package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.AnalysisRunData

import scala.collection.mutable

object AnalysisRegistry {

  private val analysisLookup:  mutable.Map[String, AnalysisImplementationDescriptor] = new mutable.HashMap()

  private val regularAnalysisLookup: mutable.Map[String, () => AnalysisImplementation] = new mutable.HashMap()
  private val incrementalAnalysisLookup: mutable.Map[String, Option[AnalysisRunData] => AnalysisImplementation] = new mutable.HashMap()

  private def combine(str1: String, str2: String) = s"$str1:$str2"
  def registerRegularAnalysis(descriptor: AnalysisImplementationDescriptor, creator: () => AnalysisImplementation): Unit = {
    if(!hasAnalysisImplementation(descriptor)) {
      assert(!descriptor.isIncremental)
      analysisLookup.put(descriptor.fullName, descriptor)
      regularAnalysisLookup.put(descriptor.fullName, creator)
    } else
      throw new IllegalStateException(s"Duplicate Analysis registration: ${descriptor.fullName} already exists")
  }

  def registerIncrementalAnalysis(descriptor: AnalysisImplementationDescriptor, creator: Option[AnalysisRunData] => AnalysisImplementation): Unit = {
    if(!hasAnalysisImplementation(descriptor)) {
      assert(descriptor.isIncremental)
      analysisLookup.put(descriptor.fullName, descriptor)
      incrementalAnalysisLookup.put(descriptor.fullName, creator)
    } else
      throw new IllegalStateException(s"Duplicate Analysis registration: ${descriptor.fullName} already exists")
  }

  private def hasAnalysisImplementation(descriptor: AnalysisImplementationDescriptor): Boolean = analysisLookup.contains(descriptor.fullName)

  def hasRegularAnalysisImplementation(analysisName: String, analysisVersion: String): Boolean =
    regularAnalysisLookup.contains(combine(analysisName, analysisVersion))

  def hasIncrementalAnalysisImplementation(analysisName: String, analysisVersion: String): Boolean =
    incrementalAnalysisLookup.contains(combine(analysisName, analysisVersion))

  def getRegularAnalysisImplementation(analysisName: String, analysisVersion: String): AnalysisImplementation =
    regularAnalysisLookup(combine(analysisName, analysisVersion))()

  def getIncrementalAnalysisImplementation(analysisName: String, analysisVersion: String, baseline: Option[AnalysisRunData]): AnalysisImplementation = {
    incrementalAnalysisLookup(combine(analysisName, analysisVersion))(baseline)
  }

  def allAnalysisDescriptors(): Iterable[AnalysisImplementationDescriptor] = analysisLookup.values

}
