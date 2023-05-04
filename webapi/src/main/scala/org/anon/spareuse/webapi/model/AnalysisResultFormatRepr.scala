package org.anon.spareuse.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

case class AnalysisResultFormatRepr(Description: String, FormatDefinition: String)

trait AnalysisResultFormatReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def analysisResultFormatReprFormat: JsonFormat[AnalysisResultFormatRepr] = jsonFormat2(AnalysisResultFormatRepr)
}
