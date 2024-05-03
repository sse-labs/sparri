package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.formats.ObjectResultFormat
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisImplementationDescriptor, AnalysisResult}

import scala.util.{Failure, Success, Try}

class MvnTaintCandidatesFinderAnalysisImpl(dataAccessor: DataAccessor) extends AnalysisImplementation {

  override val descriptor: AnalysisImplementationDescriptor = MvnTaintCandidatesFinderAnalysisImpl

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {
    // No requirements for config as of now
    inputs
      .forall(_.isProgram)
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = Try {
    var cnt = 0

    inputs.collect {
      case jp: JavaProgram =>
        cnt += 1
        log.info(s"Processing program $cnt / ${inputs.size}: ${jp.programName}")
        analyzeProgram(jp, rawConfig)
    }.flatten.toSet
  }

  private[impl] def analyzeProgram(jp: JavaProgram, config: String): Option[AnalysisResult] = {
    val actualDependencies = getTransitiveDependencies(dataAccessor, jp)
      .get
      .filter(_.scope != "test")
      .map(_.identifier)
      .toSet

    val projectIdent = MavenIdentifier
      .fromGAV(jp.programName)
      .get

    log.info(s"Initializing OPAL project for ${jp.programName} with ${actualDependencies.size} dependencies ... ")

    buildOpalProjectFor(projectIdent, actualDependencies) match {
      case Success(opalProject) =>
        log.info(s"Done initializing OPAL project for ${jp.programName}.")
        ???
      case Failure(ex) =>
        log.error(s"Failed to initialize Whole-Program-Analysis for ${jp.programName} with ${actualDependencies.size} transitive build dependencies", ex)
        None
    }
  }
}

object MvnTaintCandidatesFinderAnalysisImpl extends AnalysisImplementationDescriptor {

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis("mvn-taint-candidates-finder",
    "1.0.0",
    "Analysis that identifies potential candidate libraries that may contain dataflow leaks. Such analyses are webcontainers that use the File API, the ProcessBuilder or the SQL API",
    "OPAL 5.0.0",
    Set("Java", "Scala"),
    ObjectResultFormat(), //TODO: Actual result format!
    SoftwareEntityKind.Program,
    doesBatchProcessing = true,
    isIncremental = false)

  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Program
}
