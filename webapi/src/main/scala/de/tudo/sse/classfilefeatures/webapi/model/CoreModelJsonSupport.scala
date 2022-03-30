package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import de.tudo.sse.classfilefeatures.common.model.FieldAccessTypes.FieldAccessType
import de.tudo.sse.classfilefeatures.common.model.InvocationTypes.InvocationType
import de.tudo.sse.classfilefeatures.common.model.{ClassFileRepresentation, FieldAccessInstructionRepresentation, FieldAccessTypes, FieldDefinitionRepresentation, InvocationInstructionRepresentation, InvocationTypes, MethodBodyRepresentation, MethodRepresentation}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, JsonFormat, RootJsonFormat}

trait CoreModelJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {

  implicit val fieldDefFormat: JsonFormat[FieldDefinitionRepresentation] = jsonFormat3(FieldDefinitionRepresentation)

  implicit val invocationTypeFormat: JsonFormat[InvocationType] = enumFormat(InvocationTypes)

  implicit val invocationInstrRepFormat: JsonFormat[InvocationInstructionRepresentation] = jsonFormat5(InvocationInstructionRepresentation)

  implicit val fieldAccessTypeFormat: JsonFormat[FieldAccessType] = enumFormat(FieldAccessTypes)

  implicit val fieldAccessInstrRepFormat: JsonFormat[FieldAccessInstructionRepresentation] = jsonFormat4(FieldAccessInstructionRepresentation)

  implicit val methodBodyRepFormat: JsonFormat[MethodBodyRepresentation] = jsonFormat4(MethodBodyRepresentation)

  implicit val methodRepFormat: JsonFormat[MethodRepresentation] = jsonFormat4(MethodRepresentation)

  implicit val classFileRepFormat: JsonFormat[ClassFileRepresentation] = jsonFormat8(ClassFileRepresentation)

  private def enumFormat[T <: Enumeration](implicit enu: T): RootJsonFormat[T#Value] =
    new RootJsonFormat[T#Value] {
      def write(obj: T#Value): JsValue = JsString(obj.toString)
      def read(json: JsValue): T#Value = {
        json match {
          case JsString(txt) => enu.withName(txt)
          case somethingElse => throw DeserializationException(s"Expected a value from enum $enu instead of $somethingElse")
        }
      }
    }

}
