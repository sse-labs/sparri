package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisImplementationDescriptor, AnalysisResult}

import scala.util.Try

class MvnTaintCandidatesFinderAnalysisImpl extends AnalysisImplementation {

  override val descriptor: AnalysisImplementationDescriptor = MvnTaintCandidatesFinderAnalysisImpl

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = ???

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = ???
}

object MvnTaintCandidatesFinderAnalysisImpl extends AnalysisImplementationDescriptor {

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis("mvn-taint-candidates-finder",
    "1.0.0",
    "Analysis that identifies potential candidate libraries that may contain dataflow leaks. Such analyses are webcontainers that use the File API, the ProcessBuilder or the SQL API",
    "OPAL 5.0.0",
    Set("Java", "Scala"),
    ???,
    SoftwareEntityKind.Program,
    doesBatchProcessing = true,
    isIncremental = false)

  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Program
}
