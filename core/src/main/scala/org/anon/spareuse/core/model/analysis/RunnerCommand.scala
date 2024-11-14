package org.anon.spareuse.core.model.analysis

import spray.json._

trait RunnerCommand{
  val analysisName: String

  val associatedRunId: String

  val userName: String

  val inputEntityIds: Set[Long]

  val configurationRaw: String

  def updateRunId(newRunId: String): RunnerCommand


}


case class AnalysisCommand(override val analysisName: String,
                           override val associatedRunId: String,
                           override val userName: String,
                           override val inputEntityIds: Set[Long],
                           override val configurationRaw: String) extends RunnerCommand {
  def updateRunId(newRunId: String): RunnerCommand = {
    AnalysisCommand(analysisName, newRunId, userName, inputEntityIds, configurationRaw)
  }

  def asIncremental(baselineRunId: Option[String]): RunnerCommand =
    IncrementalAnalysisCommand(analysisName, associatedRunId, userName, inputEntityIds, configurationRaw, baselineRunId.getOrElse(""))
}

case class IncrementalAnalysisCommand(override val analysisName: String,
                                      override val associatedRunId: String,
                                      override val userName: String,
                                      override val inputEntityIds: Set[Long],
                                      override val configurationRaw: String,
                                      baselineRunId: String) extends RunnerCommand {
  override def updateRunId(newRunId: String): RunnerCommand = {
    IncrementalAnalysisCommand(analysisName, newRunId, userName, inputEntityIds, configurationRaw, baselineRunId)
  }
}


trait RunnerCommandJsonSupport extends DefaultJsonProtocol {

  implicit val runnerCommandFormat: JsonFormat[RunnerCommand] = new JsonFormat[RunnerCommand] {
    override def write(obj: RunnerCommand): JsValue = obj match {
      case ac: AnalysisCommand =>
        analysisCommandFormat.write(ac)
      case iac: IncrementalAnalysisCommand =>
        incrementalAnalysisCommandFormat.write(iac)
      case x@_ =>
        throw new SerializationException(s"Unsupported runner command type ${x.getClass}")
    }

    override def read(json: JsValue): RunnerCommand = json match {
      case jo: JsObject =>
        if(jo.fields.contains("baselineRunId")) incrementalAnalysisCommandFormat.read(jo)
        else analysisCommandFormat.read(jo)
      case x@_ =>
        throw DeserializationException(s"Unsupported JSON format for runner commands, Object expected but got ${x.getClass}")
    }
  }

  implicit val analysisCommandFormat: JsonFormat[AnalysisCommand] = jsonFormat5(AnalysisCommand)
  implicit val incrementalAnalysisCommandFormat: JsonFormat[IncrementalAnalysisCommand] = jsonFormat6(IncrementalAnalysisCommand)
}