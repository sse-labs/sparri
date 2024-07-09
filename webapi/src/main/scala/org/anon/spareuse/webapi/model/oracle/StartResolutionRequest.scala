package org.anon.spareuse.webapi.model.oracle

import spray.json.JsonFormat

final case class StartResolutionRequest(cc: ApplicationMethodRepr, ccPC: Int, types: Set[String])

trait StartResolutionRequestJsonSupport extends ApplicationMethodReprJsonSupport {
  implicit def startRequestJsonFormat: JsonFormat[StartResolutionRequest] = jsonFormat3(StartResolutionRequest)
}
