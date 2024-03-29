package org.anon.spareuse.execution

import org.anon.spareuse.core.model.analysis.RunnerCommand
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.execution.analyses.AnalysisImplementation

package object utils {

  /**
   * Exception class for when the validation of a StartRunCommand fails
   * @param reason Description of the reason the validation failed
   * @param analysisCmd Command that failed the validation
   * @param cause Optional throwable that caused the validation to fail
   */
  class AnalysisRunNotPossibleException(reason: String, analysisCmd: RunnerCommand, cause: Option[Throwable] = None) extends Throwable {

    override def getMessage: String = s"Execution of ${analysisCmd.analysisName} not possible: $reason" + cause.map(c => s" (Caused by ${c.getClass}").getOrElse("")

    override def getCause: Throwable = cause.orNull

  }

  object AnalysisRunNotPossibleException {

    def apply(reason: String, cause: Throwable)(implicit cmd: RunnerCommand): AnalysisRunNotPossibleException =
      new AnalysisRunNotPossibleException(reason, cmd, Some(cause))

    def apply(reason: String)(implicit cmd: RunnerCommand): AnalysisRunNotPossibleException =
      new AnalysisRunNotPossibleException(reason, cmd, None)

    def apply(reason: String, cmd: RunnerCommand, cause: Throwable): AnalysisRunNotPossibleException =
      new AnalysisRunNotPossibleException(reason, cmd, Some(cause))

  }


  trait ValidRunnerCommand {
    val runnerCommand: RunnerCommand
  }


  /**
   * Class representing a fully validated StartRunCommand that is ready for execution. Holds some values that have been
   * precomputed during validation
   * @param startCmd The original command  that passed validation
   * @param inputEntities The set of (resolved) input entities
   * @param analysisImpl A reference to the analysis implementation
   */
  case class ValidStartRunCommand(startCmd: RunnerCommand,
                                       inputEntities: Set[SoftwareEntityData],
                                       analysisImpl: AnalysisImplementation) extends ValidRunnerCommand {
    override val runnerCommand: RunnerCommand = startCmd
  }

}
