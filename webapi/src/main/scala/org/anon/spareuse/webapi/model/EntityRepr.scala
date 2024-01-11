package org.anon.spareuse.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class EntityRepr (Name: String,
                             Identifier: String,
                             Kind: String,
                             Language: String,
                             Repository: String,
                             ParentId: Option[String],
                             Hash: Option[String],
                             Children: Option[Array[EntityRepr]],
                             ThisTypeFqn: Option[String],
                             SuperTypeFqn: Option[String],
                             InterfaceTypeFqns: Option[Array[String]],
                             IsInterface: Option[Boolean],
                             IsFinal: Option[Boolean],
                             IsStatic: Option[Boolean],
                             IsAbstract: Option[Boolean],
                             Visibility: Option[String],
                             Descriptor: Option[String],
                             TargetType: Option[String],
                             MethodHash: Option[Int])

trait EntityReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit lazy val entityReprJsonFormat: JsonFormat[EntityRepr] = lazyFormat(jsonFormat19(EntityRepr))
}