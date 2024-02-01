package org.anon.spareuse.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class AnalysisResultRepr(UID: String, IsRevoked: Boolean, JsonContent: String, AffectedEntityIds: Set[String])

trait AnalysisResultReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def analysisResultReprJsonFormat: JsonFormat[AnalysisResultRepr] = jsonFormat4(AnalysisResultRepr)
}
