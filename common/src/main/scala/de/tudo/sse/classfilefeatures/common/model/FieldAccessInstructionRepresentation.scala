package de.tudo.sse.classfilefeatures.common.model

import FieldAccessTypes.FieldAccessType

case class FieldAccessInstructionRepresentation(fieldName: String,
                                                fieldTypeJvmName: String,
                                                fieldDeclaredClassFqn: String,
                                                fieldAccessType: FieldAccessType) {
  def identifier: String = "[" + fieldAccessType.toString + "] " + fieldDeclaredClassFqn + "." + fieldName + ":" + fieldTypeJvmName
}

object FieldAccessTypes extends Enumeration {

  type FieldAccessType = Value

  val Put: Value = Value("Put")
  val Get: Value = Value("Get")
  val PutStatic: Value = Value("PutStatic")
  val GetStatic: Value = Value("GetStatic")

}
