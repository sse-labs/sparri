package org.anon.spareuse.webapi.model.oracle

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class ApplicationMethodRepr(ident: MethodIdentifierRepr, isStatic: Boolean, types: Set[String], invokes: Seq[InvokeStmtRepr])

final case class MethodIdentifierRepr(declType: String, mName: String, mDescr: String)

final case class InvokeStmtRepr(declIdent: MethodIdentifierRepr, invokeType: Int, pc: Int)

trait ApplicationMethodReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit def identFormat: JsonFormat[MethodIdentifierRepr] = jsonFormat3(MethodIdentifierRepr)
  implicit def invokeStmtFormat: JsonFormat[InvokeStmtRepr] = jsonFormat3(InvokeStmtRepr)
  implicit def appMethodFormat: JsonFormat[ApplicationMethodRepr] = jsonFormat4(ApplicationMethodRepr)
}
