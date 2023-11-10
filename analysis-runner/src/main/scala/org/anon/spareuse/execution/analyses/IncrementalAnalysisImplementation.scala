package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.{AnalysisResultData, AnalysisRunData}
import org.anon.spareuse.core.model.entities.SoftwareEntityData

import scala.util.{Success, Try}

abstract class IncrementalAnalysisImplementation(protected val baselineRunOpt: Option[AnalysisRunData]) extends AnalysisImplementation {

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = {
    baselineRunOpt match {
      case Some(baselineRun) =>
        log.info(s"Running incremental analysis ${descriptor.fullName} with baseline run ${baselineRun.uid}")

        val splitExecutions = baselineRun
          .results
          .flatMap{ previousResult =>
            val currentEntitiesAffected = inputs.filter(currEnt => previousResult.affectedEntities.exists( prevEnt => isValidBaselineFor(prevEnt, currEnt)))

            if(currentEntitiesAffected.size == previousResult.affectedEntities.size) Some((previousResult, currentEntitiesAffected))
            else None
          }

        val inputsWithNoBaseline = inputs.diff(splitExecutions.flatMap(_._2).toSeq)

        log.info(s"Finished building execution plan for incremental analysis.")
        log.info(s"--- #Runs with baseline results: ${splitExecutions.size}")
        log.info(s"--- #Entities with no baseline: ${inputsWithNoBaseline.size}")
        log.info("")

        val noOfRuns = splitExecutions.size + (if (inputsWithNoBaseline.nonEmpty) 1 else 0)
        var currentRun = 1

        Try{
          splitExecutions.flatMap {
              case (baselineResult: AnalysisResultData, currentInputs) =>
                log.info(s"[RUN $currentRun / $noOfRuns] Incremental computation with ${currentInputs.size} inputs for result ${baselineResult.uid}")
                currentRun += 1
                executeIncremental(currentInputs, Some(baselineResult)).get

            } ++ {
            if(inputsWithNoBaseline.nonEmpty){
              log.info(s"[RUN $currentRun / $noOfRuns] Incremental computation without baseline for ${inputsWithNoBaseline.size} inputs")
              executeIncremental(inputsWithNoBaseline, None).get
            } else {
              Set.empty
            }

          }
        }
      case None if inputs.nonEmpty =>
        log.info(s"Running incremental analysis ${descriptor.fullName} with empty baseline.")
        executeIncremental(inputs, None)

      case _ =>
        log.info(s"No inputs supplied, analysis ${descriptor.fullName} will not be executed.")
        Success(Set.empty)
    }
  }


  protected def isValidBaselineFor(potentialBaseline: SoftwareEntityData, current: SoftwareEntityData): Boolean = {
    // Results must be valid for *part of a library* - not the library itself - to be used for incremental analysis
    if(potentialBaseline.isLibrary || current.isLibrary) false
    // Results must refer to entities of the same kind to be used for incremental computations
    else if(potentialBaseline.kind != current.kind) false
    // Results must belong to entities of the same library to be used for incremental computations
    // Note: No check for subsequent version numbers here, any result for the same library may be used
    else {
      val baselineProgram = potentialBaseline.findFirstParent(_.isLibrary)
      val currentProgramOpt = current.findFirstParent(_.isLibrary)

      baselineProgram.isDefined &&
        currentProgramOpt.isDefined &&
        baselineProgram.get.uid.equals(currentProgramOpt.get.uid)
    }
  }


  def executeIncremental(inputs: Seq[SoftwareEntityData], previousResult: Option[AnalysisResultData]): Try[Set[AnalysisResult]]

}
