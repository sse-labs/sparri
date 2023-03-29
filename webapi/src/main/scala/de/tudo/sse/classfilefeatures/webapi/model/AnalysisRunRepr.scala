package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}
final case class AnalysisRunRepr(uid: String, timeStamp: String, logs: Seq[String], configuration: String,
                                 state: String, revoked: Boolean, analysisName: String, analysisVersion: String,
                                 inputs: Seq[EntityRepr])

trait AnalysisRunReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol with EntityReprJsonSupport {
  implicit val analysisRunReprJsonFormat: JsonFormat[AnalysisRunRepr] = jsonFormat9(AnalysisRunRepr)
}