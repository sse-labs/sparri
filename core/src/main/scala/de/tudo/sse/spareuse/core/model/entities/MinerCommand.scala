package de.tudo.sse.spareuse.core.model.entities

import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommandJsonSupport, RunnerCommand}
import spray.json.{DefaultJsonProtocol, JsonFormat}

case class MinerCommand(entityReferences: Set[String], analysisToTrigger: Option[RunnerCommand])

trait MinerCommandJsonSupport extends DefaultJsonProtocol with RunnerCommandJsonSupport {

  implicit val minerCommandFormat: JsonFormat[MinerCommand] = jsonFormat2(MinerCommand)

}
