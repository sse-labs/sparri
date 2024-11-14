package org.anon.spareuse.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}
final case class AnalysisRunRepr(UID: String, TimeStamp: String, DurationMs: Long, Logs: Seq[String], Configuration: String,
                                 State: String, IsRevoked: Boolean, AnalysisName: String, AnalysisVersion: String,
                                 Inputs: Seq[EntityRepr])

trait AnalysisRunReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol with EntityReprJsonSupport {
  implicit val analysisRunReprJsonFormat: JsonFormat[AnalysisRunRepr] = jsonFormat10(AnalysisRunRepr)
}