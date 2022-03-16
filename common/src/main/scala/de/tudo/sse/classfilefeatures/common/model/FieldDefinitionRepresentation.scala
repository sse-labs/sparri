package de.tudo.sse.classfilefeatures.common.model

case class FieldDefinitionRepresentation(flags: Int, name: String, fieldTypeJvmName: String) {
  def identifier: String = name + ": " + fieldTypeJvmName
}
