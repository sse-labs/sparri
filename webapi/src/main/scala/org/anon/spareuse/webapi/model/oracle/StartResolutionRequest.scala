package org.anon.spareuse.webapi.model.oracle

import spray.json.JsonFormat

final case class StartResolutionRequest(cc: ApplicationMethodWithSummaryRepr, ccPC: Int, types: Set[String])

trait StartResolutionRequestJsonSupport extends LookupResponseJsonSupport {
  implicit def startRequestJsonFormat: JsonFormat[StartResolutionRequest] = jsonFormat3(StartResolutionRequest)
}
