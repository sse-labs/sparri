package de.tudo.classfilefeatures.common.model

import de.tudo.classfilefeatures.common.model.FieldAccessTypes.FieldAccessType

case class FieldAccessInstructionRepresentation(fieldName: String,
                                                fieldTypeJvmName: String,
                                                fieldDeclaredClassFqn: String,
                                                fieldAccessType: FieldAccessType)

object FieldAccessTypes extends Enumeration {

  type FieldAccessType = Value

  val Put, Get, PutStatic, GetStatic = Value

}
