package org.anon.spareuse.webapi.model.oracle

import spray.json.JsonFormat

final case class LookupResponse(requestId: Int, targets: Set[ApplicationMethodRepr], noDefs: Set[String])

trait LookupResponseJsonSupport extends ApplicationMethodReprJsonSupport {
  implicit val lookupResponseFormat: JsonFormat[LookupResponse] = jsonFormat3(LookupResponse)
}
