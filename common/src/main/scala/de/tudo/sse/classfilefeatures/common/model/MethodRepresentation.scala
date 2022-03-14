package de.tudo.sse.classfilefeatures.common.model

case class MethodRepresentation(flags: Int, name: String, jvmMethodDescriptor: String, body: Option[MethodBodyRepresentation])

case class MethodBodyRepresentation(maxStack: Int,
                                    maxLocals: Int,
                                    invocations: Seq[InvocationInstructionRepresentation],
                                    fieldAccesses: Seq[FieldAccessInstructionRepresentation])
