package de.tudo.sse.spareuse.core.model.analysis

import spray.json._

case class RunnerCommand(analysisName: String, runId: String, userName: String, inputEntityNames: Set[String], configurationRaw: String)


trait RunnerCommandJsonSupport extends DefaultJsonProtocol {
  implicit val runnerCommandFormat: JsonFormat[RunnerCommand] = jsonFormat5(RunnerCommand)
}