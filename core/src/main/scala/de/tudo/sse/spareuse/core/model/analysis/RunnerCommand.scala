package de.tudo.sse.spareuse.core.model.analysis

import de.tudo.sse.spareuse.core.model.analysis.RunnerCommandType.RunnerCommandType
import spray.json._

import scala.util.Try

trait RunnerCommand {
  val analysisName: String
  val userName: String

  val commandType: RunnerCommandType
}

object RunnerCommand {

  def fromJson(json: String): Try[RunnerCommand] = Try {
    val raw = json.parseJson

    val cmdObj = raw.asJsObject("Command needs to be a JSON object")

    def getVal(key: String): JsValue = cmdObj.fields.getOrElse(key, throw DeserializationException(s"Command is missing required key $key"))

    getVal("commandType") match {
      case typeNum: JsNumber if typeNum.value.intValue() == 0 || typeNum.value.intValue() == 1 =>
        val analysisName = getVal("analysisName").asInstanceOf[JsString].value
        val userName = getVal("userName").asInstanceOf[JsString].value

        if(typeNum.value == 0){
          val inputEntities = getVal("inputEntityNames").asInstanceOf[JsArray].elements.map(_.asInstanceOf[JsString].value).toSet
          val config = getVal("configurationRaw").asInstanceOf[JsString].value
          StartRunCommand(analysisName, userName, inputEntities, config)
        } else {
          val runId = getVal("runId").asInstanceOf[JsString].value
          val reason = getVal("abortReason").asInstanceOf[JsString].value
          StopRunCommand(analysisName, userName, runId, reason)
        }
      case _ =>
        throw DeserializationException("Command type invalid or missing")
    }
  }

  def toJson(cmd: RunnerCommand): String = {

    cmd match {
      case start: StartRunCommand =>
        JsObject(("analysisName", JsString(start.analysisName)),
          ("userName", JsString(start.userName)),
          ("commandType", JsNumber(start.commandType.id)),
          ("inputEntityNames", JsArray(start.inputEntityNames.toVector.map(JsString(_)))),
          ("configurationRaw", JsString(start.configurationRaw))).compactPrint

      case stop: StopRunCommand =>
        JsObject(("analysisName", JsString(stop.analysisName)),
          ("userName", JsString(stop.userName)),
          ("commandType", JsNumber(stop.commandType.id)),
          ("runId", JsString(stop.runId)),
          ("abortReason", JsString(stop.abortReason))).compactPrint
    }
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