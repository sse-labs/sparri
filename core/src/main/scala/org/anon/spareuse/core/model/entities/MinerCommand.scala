package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import org.anon.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import spray.json.{DefaultJsonProtocol, JsonFormat}

case class MinerCommand(entityReferences: Set[String], analysisToTrigger: Option[RunnerCommand])

trait MinerCommandJsonSupport extends DefaultJsonProtocol with RunnerCommandJsonSupport {

  implicit val minerCommandFormat: JsonFormat[MinerCommand] = jsonFormat2(MinerCommand)

}
