package de.tudo.sse.classfilefeatures.common.model

import de.tudo.sse.classfilefeatures.common.opal.OPALUtilities
import org.opalj.br.instructions.{GETFIELD, GETSTATIC, INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, Instruction, InvocationInstruction, PUTFIELD, PUTSTATIC}
import org.opalj.br.{ClassFile, Code, Field, Method}

trait ToModelConversion {

  def toModel(cf: ClassFile, handleFields: Boolean = false): ClassFileRepresentation = ClassFileRepresentation(
    flags = cf.accessFlags,
    majorVersion = cf.majorVersion,
    minorVersion = cf.minorVersion,
    thisTypeFqn = cf.thisType.fqn,
    superTypeFqn = cf.superclassType.map(_.fqn),
    interfacesFqn = cf.interfaceTypes.map(_.fqn),
    methodRepresentations = cf.methods.map( m => toModel(m, handleFields)),
    fieldRepresentations = if(handleFields) cf.fields.map(f => toModel(f)) else Seq.empty[FieldDefinitionRepresentation]
  )


  def toModel(m: Method, handleFields: Boolean): MethodRepresentation = MethodRepresentation(
    flags = m.accessFlags,
    name = m.name,
    jvmMethodDescriptor = m.descriptor.toJVMDescriptor,
    body = m.body.map(b => toModel(b, handleFields))
  )

  def toModel(c: Code, handleFields: Boolean): MethodBodyRepresentation = {

    val fieldAccessRepresentations = if(handleFields) {
      c
        .instructions
        .filter(OPALUtilities.isFieldAccessInstruction)
        .map(toFieldAccessRepresentation)
        .toSeq
    } else Seq.empty[FieldAccessInstructionRepresentation]

    val invocationRepresentations = c
      .instructions
      .filter(OPALUtilities.isInvocationInstruction)
      .map(_.asInvocationInstruction)
      .map(toInvocationRepresentation)
      .filter( _ != null) //This filters out invokedynamics

    MethodBodyRepresentation(
      maxStack = c.maxStack,
      maxLocals = c.maxLocals,
      invocations = invocationRepresentations,
      fieldAccesses = fieldAccessRepresentations
    )
  }

  def toInvocationRepresentation(i: InvocationInstruction): InvocationInstructionRepresentation = {

    i match {
      case static: INVOKESTATIC =>

        InvocationInstructionRepresentation(
          targetMethodName = static.name,
          targetMethodJvmDescriptor = static.methodDescriptor.toJVMDescriptor,
          targetMethodDeclaredClassFqn = static.declaringClass.fqn,
          isInterfaceInvocation = static.isInterface,
          invocationType = InvocationTypes.Static
        )

      case special: INVOKESPECIAL =>

        InvocationInstructionRepresentation(
          targetMethodName = special.name,
          targetMethodJvmDescriptor = special.methodDescriptor.toJVMDescriptor,
          targetMethodDeclaredClassFqn = special.declaringClass.fqn,
          isInterfaceInvocation = special.isInterface,
          invocationType = InvocationTypes.Special
        )

      case interface: INVOKEINTERFACE =>

        InvocationInstructionRepresentation(
          targetMethodName = interface.name,
          targetMethodJvmDescriptor = interface.methodDescriptor.toJVMDescriptor,
          targetMethodDeclaredClassFqn = interface.declaringClass.fqn,
          isInterfaceInvocation = true,
          invocationType = InvocationTypes.Interface
        )

      case virtual: INVOKEVIRTUAL =>

        InvocationInstructionRepresentation(
          targetMethodName = virtual.name,
          targetMethodJvmDescriptor = virtual.methodDescriptor.toJVMDescriptor,
          targetMethodDeclaredClassFqn = virtual.declaringClass.mostPreciseObjectType.fqn,
          isInterfaceInvocation = virtual.isInterfaceCall,
          invocationType = InvocationTypes.Virtual
        )

      case dynamic: INVOKEDYNAMIC =>
        // IMPROVE: Is there a way to handle dynamics?
        null

      case unknown =>
        throw new Exception(s"Unknown invocation instruction type: $unknown")
    }


  }

  def toFieldAccessRepresentation(i: Instruction): FieldAccessInstructionRepresentation = {
    i match {
      case get: GETFIELD =>

        FieldAccessInstructionRepresentation(
          fieldName = get.name,
          fieldTypeJvmName = get.fieldType.toJVMTypeName,
          fieldDeclaredClassFqn = get.declaringClass.fqn,
          fieldAccessType = FieldAccessTypes.Get
        )

      case put: PUTFIELD =>

        FieldAccessInstructionRepresentation(
          fieldName = put.name,
          fieldTypeJvmName = put.fieldType.toJVMTypeName,
          fieldDeclaredClassFqn = put.declaringClass.fqn,
          fieldAccessType = FieldAccessTypes.Put
        )

      case getStatic: GETSTATIC =>

        FieldAccessInstructionRepresentation(
          fieldName = getStatic.name,
          fieldTypeJvmName = getStatic.fieldType.toJVMTypeName,
          fieldDeclaredClassFqn = getStatic.declaringClass.fqn,
          fieldAccessType = FieldAccessTypes.GetStatic
        )

      case putStatic: PUTSTATIC =>

        FieldAccessInstructionRepresentation(
          fieldName = putStatic.name,
          fieldTypeJvmName = putStatic.fieldType.toJVMTypeName,
          fieldDeclaredClassFqn = putStatic.declaringClass.fqn,
          fieldAccessType = FieldAccessTypes.PutStatic
        )
    }
  }

  def toModel(f: Field): FieldDefinitionRepresentation = FieldDefinitionRepresentation(
    flags = f.accessFlags,
    name = f.name,
    fieldTypeJvmName = f.fieldType.toJVMTypeName
  )

}
