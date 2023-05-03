package org.anon.spareuse.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class AnalysisResultRepr(uid: String, isRevoked: Boolean, jsonContent: String, affectedEntityIds: Set[String])

trait AnalysisResultReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def analysisResultReprJsonFormat: JsonFormat[AnalysisResultRepr] = jsonFormat4(AnalysisResultRepr)
}
