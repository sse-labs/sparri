package de.tudo.sse.spareuse.execution.commands

import de.tudo.sse.spareuse.execution.commands.RunnerCommandType.RunnerCommandType

import scala.util.Try

trait RunnerCommand {
  val analysisName: String
  val userName: String

  val commandType: RunnerCommandType
}

object RunnerCommand {
  def fromJson(json: String): Try[RunnerCommand] = {
    ???
  }
}

case class StartRunCommand(override val analysisName: String, override val userName: String,
                           val inputEntityNames: Set[String], val configurationRaw: String) extends RunnerCommand{
  override val commandType: RunnerCommandType = RunnerCommandType.StartRun
}

case class StopRunCommand(override val analysisName: String, override val userName: String,
                          val runId: String, val abortReason: String) extends RunnerCommand {
  override val commandType: RunnerCommandType = RunnerCommandType.StopRun
}

object RunnerCommandType extends Enumeration {
  type RunnerCommandType = Value

  val StartRun: Value = Value(0)
  val StopRun: Value = Value(1)
}