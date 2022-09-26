package de.tudo.sse.spareuse.core.model.entities.conversion

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaFieldAccessType, JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaPackage, JavaProgram, JavaStatement}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import org.opalj.br.instructions.{FieldAccess, GETFIELD, GETSTATIC, INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, Instruction, PUTFIELD, PUTSTATIC}
import org.opalj.br.{ClassFile, Method}

import scala.collection.mutable

object OPALJavaConverter {

  def convertProgram(programIdent: String, repositoryIdent: String, opalClasses: List[ClassFile]): JavaProgram = {
    val program = new JavaProgram(programIdent, programIdent, repositoryIdent)

    val packages = opalClasses
      .map(_.thisType.packageName)
      .distinct
      .map(pName => new JavaPackage(pName, repositoryIdent))
      .toSet[SoftwareEntityData]

    program.setChildren(packages)

    packages.foreach { p =>
      val classes = opalClasses
        .filter(_.thisType.packageName.equals(p.name))
        .map(convertClass(_, p))
        .toSet[SoftwareEntityData]

      p.setChildren(classes)
    }

    program
  }

  def convertClass(cf: ClassFile, p: SoftwareEntityData): JavaClass = {
    val classRep = new JavaClass(cf.thisType.simpleName, cf.thisType.simpleName, cf.superclassType.map(_.fqn), p.repository)

    classRep.setChildren(cf.methods.map(convertMethod(_, classRep)).toSet)

    classRep
  }

  def convertMethod(m: Method, c: JavaClass): JavaMethod = {
    val methodRep = new JavaMethod(m.name, m.returnType.toJVMTypeName, m.parameterTypes.map(_.toJVMTypeName), c.repository)

    m.body.foreach { code =>

      val instructionsArray = code.instructions
      val instructionReps = mutable.Set.empty[JavaStatement]

      for (i <- Range(0, instructionsArray.length)) {
        val instr = instructionsArray(i)

        if (instr != null) {
          convertStatement(instr, i, methodRep) match {
            case Some(r) => instructionReps.add(r)
            case None =>
          }
        }
      }

      methodRep.setChildren(instructionReps.toSet)
    }

    methodRep
  }

  def convertStatement(i: Instruction, pc: Int, m: JavaMethod): Option[JavaStatement] = {
    i.opcode match {
      case PUTSTATIC.opcode | PUTFIELD.opcode | GETSTATIC.opcode | GETFIELD.opcode =>
        val accessInstr = i.asInstanceOf[FieldAccess]

        val fieldName = accessInstr.name
        val fieldType = accessInstr.fieldType.toJVMTypeName
        val fieldClass = accessInstr.declaringClass.fqn

        val accessType = accessInstr match {
          case _: PUTSTATIC => JavaFieldAccessType.StaticPut
          case _: GETSTATIC => JavaFieldAccessType.StaticGet
          case _: PUTFIELD => JavaFieldAccessType.InstancePut
          case _: GETFIELD => JavaFieldAccessType.InstanceGet
        }

        Some(new JavaFieldAccessStatement(fieldName, fieldType, fieldClass, accessType, pc, m.repository))

      case INVOKESTATIC.opcode | INVOKEVIRTUAL.opcode | INVOKESPECIAL.opcode | INVOKEINTERFACE.opcode | INVOKEDYNAMIC.opcode =>
        val invokeInstr = i.asInvocationInstruction

        val targetMethodName = invokeInstr.name
        val paramCount = invokeInstr.methodDescriptor.parametersCount
        val returnType = invokeInstr.methodDescriptor.returnType.toJVMTypeName

        invokeInstr match {
          case static: INVOKESTATIC =>
            Some(new JavaInvokeStatement(targetMethodName, static.declaringClass.fqn,
              paramCount, returnType, JavaInvocationType.Static, pc, m.repository))

          case special: INVOKESPECIAL =>
            Some(new JavaInvokeStatement(targetMethodName, special.declaringClass.fqn,
              paramCount, returnType, JavaInvocationType.Special, pc, m.repository))

          case virtual: INVOKEVIRTUAL =>
            Some(new JavaInvokeStatement(targetMethodName, virtual.declaringClass.toJVMTypeName,
              paramCount, returnType, JavaInvocationType.Virtual, pc, m.repository))

          case interface: INVOKEINTERFACE =>
            Some(new JavaInvokeStatement(targetMethodName, interface.declaringClass.fqn,
              paramCount, returnType, JavaInvocationType.Interface, pc, m.repository))

          case _: INVOKEDYNAMIC =>
            Some(new JavaInvokeStatement(targetMethodName, "<DYNAMIC>", paramCount,
              "<DYNAMIC>", JavaInvocationType.Dynamic, pc, m.repository))
        }

      case _ => None
    }

  }

}
