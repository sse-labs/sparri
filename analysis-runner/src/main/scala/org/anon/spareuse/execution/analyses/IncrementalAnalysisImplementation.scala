package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.{AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.utils

import scala.util.{Success, Try}

abstract class IncrementalAnalysisImplementation(protected val baselineRunOpt: Option[AnalysisRunData]) extends AnalysisImplementation {

  /**
   * Returns true if at least one affected entity of the given result is part of a previous version of the same library
   * as the given entity. The check for "previous version" is omitted if semantic versioning is not followed.
   */
  protected def isPartOfBaselineFor(currentEntity: SoftwareEntityData, result: AnalysisResultData): Boolean = {
    val currentProgram = currentEntity.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram]

    result
      .affectedEntities
      // If the previous result affects a library, it cannot be used for incremental computations
      .filterNot(_.isLibrary)
      // Previous results must be less (or equally) specific than the current one in order to be used for incremental computations
      .filterNot(prevEntity => SoftwareEntityKind.isLessSpecific(prevEntity.kind, currentEntity.kind))
      // Previous results must belong to the same library (GA) and should have a lower version number ( not necessary if semver is not used)
      .exists{ prevEntity =>
        val prevProgram = prevEntity.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram]
        val compareTry = Try(utils.compareSemanticVersions(currentProgram.v, prevProgram.v))

        currentProgram.ga.equals(prevProgram.ga) && (compareTry.isFailure || compareTry.get > 0)
      }
  }

  override final def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = {

    // We need to limit ourselves to distributive incremental analyses for now
    if(!this.descriptor.inputBatchProcessing)
      throw new IllegalStateException(s"Illegal configuration for ${this.getClass}: Incremental analyses must be distributive")

    baselineRunOpt match {
      case Some(baselineRun) =>
        log.info(s"Running incremental analysis ${descriptor.fullName} with baseline run ${baselineRun.uid}")

        val executionPlan = inputs.map { input =>
          (input, baselineRun.results.filter( prevResult => isPartOfBaselineFor(input, prevResult)))
        }

        val noInputsWithNoBaseline = executionPlan.count(_._2.isEmpty)
        val noInputsWithBaseline = executionPlan.count(_._2.nonEmpty)

        log.info(s"Finished building execution plan for incremental analysis.")
        log.info(s"--- #Runs with baseline results: $noInputsWithBaseline")
        log.info(s"--- #Entities with no baseline: $noInputsWithNoBaseline")
        log.info("")

        val noOfRuns = executionPlan.size
        var currentRun = 1

        Try{
          executionPlan.flatMap{
            case (currentInput, baselineResults) =>
              log.info(s"[RUN $currentRun / $noOfRuns] Incremental computation with ${baselineResults.size} baseline results for input ${currentInput.name}")
              currentRun += 1
              executeIncremental(currentInput, baselineResults, rawConfig).get
          }.toSet

        }

      case None if inputs.nonEmpty =>
        log.info(s"Running incremental analysis ${descriptor.fullName} with empty baseline.")
        var currentRun = 1
        Try{
          inputs.flatMap { input =>
            log.info(s"[RUN $currentRun / ${inputs.size}] Incremental computation with no baseline for input ${input.name}")
            currentRun += 1
            executeIncremental(input, Set.empty, rawConfig).get
          }.toSet
        }

      case _ =>
        log.info(s"No inputs supplied, analysis ${descriptor.fullName} will not be executed.")
        Success(Set.empty)
    }
  }


  def executeIncremental(input: SoftwareEntityData, previousResults: Set[AnalysisResultData], rawConfig: String): Try[Set[AnalysisResult]]

}
