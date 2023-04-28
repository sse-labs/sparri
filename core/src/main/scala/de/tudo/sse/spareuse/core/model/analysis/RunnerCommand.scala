package de.tudo.sse.spareuse.core.model.analysis

import spray.json._

case class RunnerCommand(analysisName: String, runId: String, userName: String, inputEntityNames: Set[String], configurationRaw: String){
  def updateRunId(newRunId: String): RunnerCommand = {
    RunnerCommand(analysisName, newRunId, userName, inputEntityNames, configurationRaw)
  }
}


trait RunnerCommandJsonSupport extends DefaultJsonProtocol {
  implicit val runnerCommandFormat: JsonFormat[RunnerCommand] = jsonFormat5(RunnerCommand)
}