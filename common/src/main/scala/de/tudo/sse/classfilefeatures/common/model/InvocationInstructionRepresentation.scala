package de.tudo.sse.classfilefeatures.common.model

import InvocationTypes.InvocationType

case class InvocationInstructionRepresentation(targetMethodName: String,
                                               targetMethodJvmDescriptor: String,
                                               targetMethodDeclaredClassFqn: String,
                                               isInterfaceInvocation: Boolean,
                                               invocationType: InvocationType) {
  def identifier: String = "[" + invocationType.toString + "] " + targetMethodDeclaredClassFqn + "." + targetMethodName + targetMethodJvmDescriptor + "[Interface=" + isInterfaceInvocation.toString + "]"
}

object InvocationTypes extends Enumeration {

  type InvocationType = Value

  val Static: Value = Value("Static")
  val Special: Value = Value("Special")
  val Interface: Value = Value("Interface")
  val Virtual: Value = Value("Virtual")
}