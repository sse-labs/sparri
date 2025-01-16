package org.anon.spareuse.webapi.model.oracle

import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSMethodRepJsonFormat
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.FactRep
import spray.json.JsonFormat

final case class IFDSQueryRequest(entryMethod: MethodIdentifierRepr, factsActive: Set[FactRep])

trait IFDSQueryRequestJsonSupport extends ApplicationMethodReprJsonSupport with DefaultIFDSMethodRepJsonFormat {
  implicit def queryRequestFormat: JsonFormat[IFDSQueryRequest] = jsonFormat2(IFDSQueryRequest)
}