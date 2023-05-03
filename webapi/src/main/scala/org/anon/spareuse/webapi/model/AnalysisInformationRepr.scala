package org.anon.spareuse.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

//TODO: Schema Typ
final case class AnalysisInformationRepr(Name: String, Description: String, Version: String, IsRevoked: Boolean, InputEntityKind: String,
                                         InputLanguages: Array[String], TechnologyInformation: Option[String], User: String, ResultSchema: String, RunIds: Array[String])


trait AnalysisInformationReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def analysisInformationReprFormat: JsonFormat[AnalysisInformationRepr] = jsonFormat10(AnalysisInformationRepr)
}
