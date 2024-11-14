package org.anon.spareuse.webapi.model.oracle

import spray.json.JsonFormat

final case class InitializeResolutionRequest(libs: Set[String], types: Set[TypeNodeRepr], initTypes: Set[String], jreVersion: Option[String], mode: Int)

trait InitializeResolutionRequestJsonSupport extends TypeNodeReprJsonSupport {
  implicit val initRequestJsonFormat: JsonFormat[InitializeResolutionRequest] = jsonFormat5(InitializeResolutionRequest)
}
