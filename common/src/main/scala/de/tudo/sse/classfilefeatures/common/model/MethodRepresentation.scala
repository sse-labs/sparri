package de.tudo.sse.classfilefeatures.common.model

case class MethodRepresentation(flags: Int, name: String, jvmMethodDescriptor: String, body: Option[MethodBodyRepresentation]){
  def identifier: String = name + jvmMethodDescriptor + "[" + (if(body.isDefined) "BODY" else "ABSTRACT") + "]"
}

case class MethodBodyRepresentation(maxStack: Int,
                                    maxLocals: Int,
                                    invocations: Seq[InvocationInstructionRepresentation],
                                    fieldAccesses: Seq[FieldAccessInstructionRepresentation])
