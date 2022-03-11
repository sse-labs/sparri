package de.tudo.classfilefeatures.common.model

import de.tudo.classfilefeatures.common.model.InvocationTypes.InvocationType

case class InvocationInstructionRepresentation(targetMethodName: String,
                                               targetMethodJvmDescriptor: String,
                                               targetMethodDeclaredClassFqn: String,
                                               isInterfaceInvocation: Boolean,
                                               invocationType: InvocationType)

object InvocationTypes extends Enumeration {

  type InvocationType = Value

  val Static, Special, Interface, Virtual = Value
}