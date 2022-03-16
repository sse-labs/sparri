package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.InvocationInstructionRepresentation
import de.tudo.sse.classfilefeatures.common.model.InvocationTypes.InvocationType

class LibraryInvocationInstructionModel(prototypeElement: InvocationInstructionRepresentation) extends ConditionallyActiveElement[InvocationInstructionRepresentation] {

  val targetMethodName: String = prototypeElement.targetMethodName
  val targetMethodJvmDescriptor: String = prototypeElement.targetMethodJvmDescriptor
  val targetMethodDeclaredClassFqn: String = prototypeElement.targetMethodDeclaredClassFqn
  val isInterfaceInvocation: Boolean = prototypeElement.isInterfaceInvocation
  val invocationType: InvocationType = prototypeElement.invocationType

  override val identifier: String = prototypeElement.identifier

  // All model information is part of the identifier, so there is nothing to update!
  override protected def updateModel(release: String, element: InvocationInstructionRepresentation): Unit = {}
}
