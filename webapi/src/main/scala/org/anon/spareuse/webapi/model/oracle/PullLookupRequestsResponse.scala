package org.anon.spareuse.webapi.model.oracle

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.LookupRequestRepresentation
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class PullLookupRequestsResponse(isInitialized: Boolean, isResolving: Boolean, requests: Set[LookupRequestRepresentation], hasFailed: Boolean, fatalError: Option[String])

trait PullLookupRequestsResponseJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val lookupRequestFormat: JsonFormat[LookupRequestRepresentation] = jsonFormat5(LookupRequestRepresentation)
  implicit val pullResponseFormat: JsonFormat[PullLookupRequestsResponse] = jsonFormat5(PullLookupRequestsResponse)
}
