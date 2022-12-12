package de.tudo.sse.spareuse.core.model.analysis

import spray.json._

case class RunnerCommand(analysisName: String, userName: String, inputEntityNames: Set[String], configurationRaw: String)


trait RunnerCommandJsonSupport extends DefaultJsonProtocol {
  implicit val runnerCommandFormat: JsonFormat[RunnerCommand] = jsonFormat4(RunnerCommand)
}