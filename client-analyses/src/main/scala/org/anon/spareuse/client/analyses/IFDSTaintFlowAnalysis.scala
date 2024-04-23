package org.anon.spareuse.client.analyses

import org.anon.spareuse.execution.analyses.impl.ifds.IFDSTaintFlowSummaryBuilderImpl

import java.io.File
import scala.util.Try

class IFDSTaintFlowAnalysis(classesDirectory: File, pomFile: File) extends ClientAnalysis(classesDirectory, pomFile) {

  private val remoteAnalysisName: String = IFDSTaintFlowSummaryBuilderImpl.analysisName
  private val remoteAnalysisVersion: String = "0.0.1"

  override protected[analyses] def requirements: Seq[AnalysisRequirement] =  Seq(AnalysisRequirement("org.apache.maven.indexer:indexer-core!org.apache.maven.indexer:indexer-core:6.0.0", remoteAnalysisName, remoteAnalysisVersion),
    AnalysisRequirement("ch.qos.logback:logback-classic!ch.qos.logback:logback-classic:1.4.4", remoteAnalysisName, remoteAnalysisVersion)) //TODO: Use real code once dependency extraction works
    /*getAllDependencies
      .map(dep => AnalysisRequirement(dep.identifier.toString, remoteAnalysisName, remoteAnalysisVersion))
      .toSeq*/

  def execute(): Try[Unit] = Try {
    val p = getOpalProject(loadJre = false)

    val projectModel = getProjectModel(p.allProjectClassFiles)

    //TODO: Do something
  }
}
