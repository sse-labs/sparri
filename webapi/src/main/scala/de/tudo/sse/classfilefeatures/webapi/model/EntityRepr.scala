package de.tudo.sse.classfilefeatures.webapi.model

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
                             ReturnType: Option[String],
                             ParameterTypes: Option[Array[String]])

trait EntityReprJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit lazy val entityReprJsonFormat: JsonFormat[EntityRepr] = lazyFormat(jsonFormat13(EntityRepr))
}