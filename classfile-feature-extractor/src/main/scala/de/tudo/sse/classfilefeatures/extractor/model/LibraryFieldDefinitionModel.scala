package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.FieldDefinitionRepresentation

class LibraryFieldDefinitionModel(prototypeElement: FieldDefinitionRepresentation) extends ConditionallyActiveElement[FieldDefinitionRepresentation]{

  val fieldName: String = prototypeElement.name
  val fieldTypeJvmName: String = prototypeElement.fieldTypeJvmName

  val flagsEvolution: ValueEvolution[Int] = new ValueEvolution[Int]

  override val identifier: String = prototypeElement.identifier

  override protected def updateModel(release: String, element: FieldDefinitionRepresentation): Unit = {
    flagsEvolution.addValueAt(release, element.flags)
  }
}
