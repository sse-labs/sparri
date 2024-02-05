package org.anon.spareuse.client.analyses

import org.anon.spareuse.execution.analyses.impl.ifds.IFDSTaintFlowSummaryBuilderImpl

import java.io.File

class IFDSTaintFlowAnalysis(classesDirectory: File, pomFile: File) extends ClientAnalysis(classesDirectory, pomFile) {

  private val remoteAnalysisName: String = IFDSTaintFlowSummaryBuilderImpl.analysisName
  private val remoteAnalysisVersion: String = "0.0.1"

  override protected[analyses] def requirements: Seq[AnalysisRequirement] =
    getAllDependencies
      .map(dep => AnalysisRequirement(dep.identifier.toString, remoteAnalysisName, remoteAnalysisVersion))
      .toSeq
}
