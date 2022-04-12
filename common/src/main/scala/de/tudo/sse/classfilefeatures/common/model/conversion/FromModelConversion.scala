package de.tudo.sse.classfilefeatures.common.model.conversion

import de.tudo.sse.classfilefeatures.common.model._
import org.opalj.bi.{ACC_PRIVATE, ACC_STATIC}
import org.opalj.br.instructions.{ACONST_NULL, ARETURN, BIPUSH, ConstantLengthInstruction, DLOAD, DRETURN, FLOAD, FRETURN, GETFIELD, GETSTATIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, IRETURN, Instruction, InvocationInstruction, LLOAD, LRETURN, NEW, POP, POP2, PUTFIELD, PUTSTATIC, RETURN}
import org.opalj.br._
import org.opalj.collection.immutable.RefArray

import scala.collection.mutable

trait FromModelConversion {

  def typeIsInterface(typeFqn: String): Boolean

  def supportSimpleAi: Boolean

  def toDummyClassFile(cfr: ClassFileRepresentation): ClassFile = {

    val artificialAiFields = if (supportSimpleAi) {

      val aiSupportArtificialFields = new mutable.HashMap[String, FieldDefinitionRepresentation]

      cfr
        .methodRepresentations
        .flatMap(m => m.body.map(b => b.invocations).getOrElse(Seq.empty))
        .filter(i => typeIsInterface(i.targetMethodDeclaredClassFqn))
        .foreach { instruction =>

          if (!aiSupportArtificialFields.contains(instruction.targetMethodDeclaredClassFqn)) {
            val fieldName = s"artificial_ai_support_field_${aiSupportArtificialFields.size}"

            //TODO: Does the mix of ObjectTypeFQN and FieldName work?
            val artificialField = FieldDefinitionRepresentation(ACC_PRIVATE.mask & ACC_STATIC.mask,
              fieldName, instruction.targetMethodDeclaredClassFqn)

            aiSupportArtificialFields.put(instruction.targetMethodDeclaredClassFqn, artificialField)
          }

        }

      aiSupportArtificialFields.toMap
    } else {
      Map.empty[String, FieldDefinitionRepresentation]
    }

    val fieldsDef = RefArray.mapFrom(cfr.fieldRepresentations ++ artificialAiFields.values)(toDummyFieldDefinition)

    ClassFile(
      majorVersion = cfr.majorVersion,
      minorVersion = cfr.minorVersion,
      accessFlags = cfr.flags,
      thisType = ObjectType(cfr.thisTypeFqn),
      superclassType = cfr.superTypeFqn.map(ObjectType(_)),
      interfaceTypes = RefArray.mapFrom(cfr.interfacesFqn)(ObjectType(_)),
      fields = fieldsDef,
      methods = RefArray.mapFrom(cfr.methodRepresentations)(mr => toDummyMethod(mr, cfr.thisTypeFqn, artificialAiFields)),
      // IMPROVE: Make sure that no CF-Level attribute has any effect on CG generation
      attributes = NoAttributes
    )

  }

  def toDummyMethod(mr: MethodRepresentation,
                    currentClassFqn: String,
                    artificialFieldsLookup: Map[String, FieldDefinitionRepresentation]): MethodTemplate = {
    val descriptor = MethodDescriptor(mr.jvmMethodDescriptor)

    // IMPROVE: Make sure no attribute other than Code is needed for CG generation
    val attributes = mutable.ListBuffer.empty[Attribute]

    if (mr.body.isDefined) {
      attributes.append(toDummyCodeAttribute(mr.body.get, descriptor, currentClassFqn, artificialFieldsLookup))
    }


    Method(accessFlags = mr.flags,
      name = mr.name,
      descriptor = descriptor,
      attributes = RefArray.from(attributes.toArray))
  }

  def toDummyFieldDefinition(fdr: FieldDefinitionRepresentation): FieldTemplate =
    Field(fdr.flags, fdr.name, FieldType(fdr.fieldTypeJvmName), NoAttributes)

  def toDummyCodeAttribute(mbr: MethodBodyRepresentation,
                           descriptor: MethodDescriptor,
                           currentClassFqn: String,
                           artificialFieldsLookup: Map[String, FieldDefinitionRepresentation]): Code = {

    val instructionSeq = mutable.ListBuffer.empty[Instruction]

    def appendInstructions(instructions: Seq[Instruction]): Unit = instructions.foreach(instructionSeq.append(_))

    def appendInstruction(instruction: ConstantLengthInstruction): Unit = appendInstructions(withPadding(instruction))

    // Push a dummy value for the object reference on the stack. Non-Static invocations and field-accesses are performed
    // on an object, which must be present on the stack before the parameters are pushed / the field access happens.
    // There are two main cases:
    //  - If the declared class of the object is not an interface, we can just push a NEW instruction for this type
    //  - If the declared class is known to be an interface, we cannot push NEW, as interfaces cannot be instantiated.
    //    For these cases we introduced artificial fields to the class, which have the desired interface type and are both
    //    static and private. We then use a GETSTATIC instruction to access this artificial field, thus creating a valid
    //    object reference to the interface type.
    //
    // Note that all of this is required since OPAL performs some basic AI even for CG algorithms like CHA. Without these
    // modifications, OPAL will discard a lot of possible edges
    def appendValidObjectReference(classFqn: String): Unit = if(supportSimpleAi && typeIsInterface(classFqn)){
      val artificialFieldDef = artificialFieldsLookup(classFqn)
      appendInstruction(GETSTATIC(currentClassFqn, artificialFieldDef.name, ObjectType(artificialFieldDef.fieldTypeJvmName).toJVMTypeName))
    } else {
      appendInstruction(NEW(classFqn))
    }

    // Methods
    mbr.invocations.reverse.foreach { iir =>
      // Recreate the original instruction from it's representation
      val dummyInstruction = toDummyInstruction(iir)

      // For non-static invocations we have to push the object reference onto the stack
      if (!iir.invocationType.equals(InvocationTypes.Static)) {
        appendValidObjectReference(iir.targetMethodDeclaredClassFqn)
      }

      // We always want to push dummy parameters for the method invocation onto the stack
      dummyInstruction.methodDescriptor.parameterTypes.foreach { pType =>
        getDummyValueCreatingInstructionsFor(pType).foreach(i => instructionSeq.append(i))
      }

      // Now push the dummy invocation instruction
      appendInstruction(dummyInstruction)

      // Pop return value of invocation from stack. Keep in mind that values of computational type 2 need a POP2 instruction
      if (!dummyInstruction.methodDescriptor.returnType.isVoidType && dummyInstruction.methodDescriptor.returnType.computationalType.isCategory2) {
        appendInstruction(POP2)
      } else if (!dummyInstruction.methodDescriptor.returnType.isVoidType) {
        appendInstruction(POP)
      }
    }

    mbr.fieldAccesses.foreach { fair =>

      // For non-static fields: Put object ref on stack
      if (fair.fieldAccessType.equals(FieldAccessTypes.Get) || fair.fieldAccessType.equals(FieldAccessTypes.Put)) {
        appendValidObjectReference(fair.fieldDeclaredClassFqn)
      }

      // For PUT: Put dummy value on stack
      if (fair.fieldAccessType.equals(FieldAccessTypes.Put) || fair.fieldAccessType.equals(FieldAccessTypes.PutStatic)) {
        val fieldType = FieldType(fair.fieldTypeJvmName)

        // Build appropriate dummy value
        if (fieldType.isObjectType) {
          appendValidObjectReference(fieldType.asObjectType.fqn)
        } else if (fieldType.isBaseType) {
          getDummyValueCreatingInstructionsFor(fieldType.asBaseType).foreach(i => instructionSeq.append(i))
        } else {
          // This happens for arrays
          appendInstruction(NEW("java/lang/Object"))
        }
      }

      appendInstruction(toDummyFieldInstruction(fair))
    }


    getDummyValueCreatingInstructionsFor(descriptor.returnType).foreach(i => instructionSeq.append(i))
    appendInstruction(getReturnInstructionFor(descriptor.returnType))

    Code(maxStack = mbr.maxStack,
      maxLocals = mbr.maxLocals,
      instructions = instructionSeq.toArray,
      exceptionHandlers = NoExceptionHandlers,
      attributes = NoAttributes
    )
  }

  def toDummyFieldInstruction(fair: FieldAccessInstructionRepresentation): ConstantLengthInstruction = {
    fair.fieldAccessType match {
      case FieldAccessTypes.Get =>
        GETFIELD(fair.fieldDeclaredClassFqn, fair.fieldName, fair.fieldTypeJvmName)
      case FieldAccessTypes.Put =>
        PUTFIELD(fair.fieldDeclaredClassFqn, fair.fieldName, fair.fieldTypeJvmName)
      case FieldAccessTypes.PutStatic =>
        PUTSTATIC(fair.fieldDeclaredClassFqn, fair.fieldName, fair.fieldTypeJvmName)
      case _ =>
        GETSTATIC(fair.fieldDeclaredClassFqn, fair.fieldName, fair.fieldTypeJvmName)
    }
  }

  def toDummyInstruction(iir: InvocationInstructionRepresentation): InvocationInstruction = iir.invocationType match {
    case InvocationTypes.Static =>
      INVOKESTATIC(iir.targetMethodDeclaredClassFqn, iir.isInterfaceInvocation, iir.targetMethodName, iir.targetMethodJvmDescriptor)
    case InvocationTypes.Special =>
      INVOKESPECIAL(iir.targetMethodDeclaredClassFqn, iir.isInterfaceInvocation, iir.targetMethodName, iir.targetMethodJvmDescriptor)
    case InvocationTypes.Interface =>
      INVOKEINTERFACE(iir.targetMethodDeclaredClassFqn, iir.targetMethodName, iir.targetMethodJvmDescriptor)
    case InvocationTypes.Virtual =>
      INVOKEVIRTUAL(iir.targetMethodDeclaredClassFqn, iir.targetMethodName, iir.targetMethodJvmDescriptor)
    case u@_ =>
      throw new Exception(s"Unknown instruction type while creating dummy instruction: ${u.getClass}")

  }


  private def withPadding(instruction: ConstantLengthInstruction): Seq[Instruction] = {
    Seq(instruction) ++ Range(1, instruction.length).map(_ => null)
  }

  private def getReturnInstructionFor(typeVal: Type): ConstantLengthInstruction = typeVal match {
    case BooleanType | ByteType | CharType | ShortType | IntegerType => IRETURN
    case FloatType => FRETURN
    case LongType => LRETURN
    case DoubleType => DRETURN
    case VoidType => RETURN
    case _ => ARETURN
  }


  private def getDummyValueCreatingInstructionsFor(typeVal: Type): Seq[Instruction] = typeVal match {
    case BooleanType | ByteType | CharType | ShortType | IntegerType =>
      Seq(BIPUSH(0), null)
    case FloatType =>
      Seq(FLOAD(0), null)
    case LongType =>
      Seq(LLOAD(0), null)
    case DoubleType =>
      Seq(DLOAD(0), null)
    case VoidType =>
      Seq.empty
    case _ =>
      Seq(ACONST_NULL)
  }

}
