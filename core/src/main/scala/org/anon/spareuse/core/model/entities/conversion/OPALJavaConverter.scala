package org.anon.spareuse.core.model.entities.conversion

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaFieldAccessType, JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaPackage, JavaProgram, JavaStatement, JavaNewInstanceStatement}
import org.anon.spareuse.core.model.entities.JavaEntities
import org.opalj.ba
import org.opalj.bc.Assembler
import org.opalj.br.instructions.{FieldAccess, GETFIELD, GETSTATIC, INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, Instruction, NEW, PUTFIELD, PUTSTATIC}
import org.opalj.br.{ClassFile, Method}

import java.security.MessageDigest
import scala.collection.mutable

object OPALJavaConverter {

  private def hashBytes(bytes: Array[Byte]): Array[Byte] = MessageDigest.getInstance("md5").digest(bytes)

  private def hashClass(cf: ClassFile): Array[Byte] = hashBytes(Assembler(ba.toDA(cf)))

  def convertProgram(programIdent: String,
                     repositoryIdent: String,
                     opalClasses: List[ClassFile],
                     loadClassContents: Boolean = true): JavaProgram = {

    val classHashes = opalClasses.map(cf => (cf, hashClass(cf))).toMap

    // Program hash is (for now) defined as the hash of all class hashes (this excludes resources / manifest changes)
    val programHash = hashBytes(classHashes.values.toArray.flatten)

    val program = JavaEntities.buildProgram(programIdent, repositoryIdent, programHash)

    val packages = opalClasses
      .map(_.thisType.packageName)
      .distinct
      .map(pName => JavaEntities.buildPackageFor(program, pName))

    packages.foreach { p =>
      opalClasses
        .filter(_.thisType.packageName.equals(p.name))
        .foreach(cf => addClass(cf, p, classHashes(cf), loadClassContents))
    }

    program
  }

  def addClass(cf: ClassFile, p: JavaPackage, classHash: Array[Byte], loadClassContents: Boolean = true): JavaClass = {
    val classRep = JavaEntities.buildClassFor(p, cf.thisType.simpleName, cf.thisType.fqn, cf.isInterfaceDeclaration, cf.isFinal, cf.isAbstract, cf.superclassType.map(_.fqn), cf.interfaceTypes.map(_.fqn).toSet, classHash)

    if(loadClassContents)
      cf.methods.foreach(addMethod(_, classRep))

    classRep
  }

  def buildMethodHash(m: Method): Int = {
    val bodyHash = m
      .body
      .map { code =>
        var sum = 0
        code.foreach { pcAndInstr =>
          sum += 5 * pcAndInstr.pc + 29 * pcAndInstr.instruction.toString(pcAndInstr.pc).hashCode
        }
        sum
      }
      .getOrElse(0)

    97 * m.classFile.thisType.fqn.hashCode + 73 * m.name.hashCode + 47 * m.descriptor.hashCode + 23 * bodyHash
  }

  def addMethod(m: Method, c: JavaClass): JavaMethod = {

    val methodRep = JavaEntities.buildMethodFor(c, m.name, m.returnType.toJVMTypeName, m.parameterTypes.map(_.toJVMTypeName),
      m.isFinal, m.isStatic, m.isAbstract, m.visibilityModifier.map(_.javaName.get).getOrElse("default"), buildMethodHash(m))

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

    val ident = m.uid + "!" + String.valueOf(pc)

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

        Some(new JavaFieldAccessStatement(fieldName, fieldType, fieldClass, accessType, pc, ident, m.repository))

      case INVOKESTATIC.opcode | INVOKEVIRTUAL.opcode | INVOKESPECIAL.opcode | INVOKEINTERFACE.opcode | INVOKEDYNAMIC.opcode =>
        val invokeInstr = i.asInvocationInstruction

        val targetMethodName = invokeInstr.name
        val paramTypes = invokeInstr.methodDescriptor.parameterTypes.map(_.toJVMTypeName)
        val returnType = invokeInstr.methodDescriptor.returnType.toJVMTypeName

        invokeInstr match {
          case static: INVOKESTATIC =>
            Some(new JavaInvokeStatement(targetMethodName, static.declaringClass.fqn,
              paramTypes, returnType, JavaInvocationType.Static, pc, ident, m.repository))

          case special: INVOKESPECIAL =>
            Some(new JavaInvokeStatement(targetMethodName, special.declaringClass.fqn,
              paramTypes, returnType, JavaInvocationType.Special, pc, ident, m.repository))

          case virtual: INVOKEVIRTUAL =>
            Some(new JavaInvokeStatement(targetMethodName, virtual.declaringClass.toJVMTypeName,
              paramTypes, returnType, JavaInvocationType.Virtual, pc, ident, m.repository))

          case interface: INVOKEINTERFACE =>
            Some(new JavaInvokeStatement(targetMethodName, interface.declaringClass.fqn,
              paramTypes, returnType, JavaInvocationType.Interface, pc, ident, m.repository))

          case _: INVOKEDYNAMIC =>
            Some(new JavaInvokeStatement(targetMethodName, "<DYNAMIC>", paramTypes,
              "<DYNAMIC>", JavaInvocationType.Dynamic, pc, ident, m.repository))
        }

      case NEW.opcode =>
        val newInstr = i.asNEW
        Some(new JavaNewInstanceStatement(newInstr.objectType.fqn, pc, ident, m.repository))

      case _ => None
    }

  }

}
