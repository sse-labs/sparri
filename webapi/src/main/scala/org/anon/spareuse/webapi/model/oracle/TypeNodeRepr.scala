package org.anon.spareuse.webapi.model.oracle

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class TypeNodeRepr(fqn: String, superFqn: Option[String], interfaceFqns: Set[String], isInterface: Boolean)

trait TypeNodeReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val nodeReprFormat: JsonFormat[TypeNodeRepr] = jsonFormat4(TypeNodeRepr)
}


