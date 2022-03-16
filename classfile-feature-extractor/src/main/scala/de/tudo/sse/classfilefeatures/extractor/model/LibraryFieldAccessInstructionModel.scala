package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.FieldAccessInstructionRepresentation
import de.tudo.sse.classfilefeatures.common.model.FieldAccessTypes.FieldAccessType

class LibraryFieldAccessInstructionModel(private val prototypeElement: FieldAccessInstructionRepresentation) extends ConditionallyActiveElement[FieldAccessInstructionRepresentation] {

  val fieldName: String = prototypeElement.fieldName
  val fieldTypeJvmName: String = prototypeElement.fieldTypeJvmName
  val fieldDeclaredClassFqn: String = prototypeElement.fieldDeclaredClassFqn
  val fieldAccessType: FieldAccessType = prototypeElement.fieldAccessType

  override val identifier: String = prototypeElement.identifier

  // All model information is part of the identifier, so there is nothing to update!
  override protected def updateModel(release: String, element: FieldAccessInstructionRepresentation): Unit = {}
}
