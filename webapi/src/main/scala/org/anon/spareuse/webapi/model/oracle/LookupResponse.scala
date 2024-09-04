package org.anon.spareuse.webapi.model.oracle

import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSMethodRepJsonFormat
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.MethodIFDSRep
import spray.json.JsonFormat

final case class ApplicationMethodWithSummaryRepr(methodRep: ApplicationMethodRepr, summaryRepr: MethodIFDSRep)

final case class LookupResponse(requestId: Int, targets: Set[ApplicationMethodWithSummaryRepr], noDefs: Set[String], hasFatalErrors: Boolean)

trait LookupResponseJsonSupport extends ApplicationMethodReprJsonSupport with DefaultIFDSMethodRepJsonFormat {
  implicit val applicationMethodWithSummaryReprFormat: JsonFormat[ApplicationMethodWithSummaryRepr] = jsonFormat2(ApplicationMethodWithSummaryRepr)
  implicit val lookupResponseFormat: JsonFormat[LookupResponse] = jsonFormat4(LookupResponse)
}
