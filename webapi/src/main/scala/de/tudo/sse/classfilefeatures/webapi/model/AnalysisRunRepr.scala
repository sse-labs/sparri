package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

//TODO
final case class AnalysisRunRepr()

trait AnalysisRunReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val analysisRunReprJsonFormat: JsonFormat[AnalysisRunRepr] = ???
}