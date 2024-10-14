package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaFieldAccessType, JavaInvocationType, JavaInvokeStatement, JavaLibrary, JavaMethod, JavaNewInstanceStatement, JavaPackage, JavaProgram}
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaFieldAccessRepr, JavaInvocationRepr, JavaMethodRepr, JavaProgramRepr}
import org.anon.spareuse.core.utils.fromHex

object JavaConverter {



  def toLib(repr: SoftwareEntityRepr): JavaLibrary = new JavaLibrary(repr.name, repr.repository)

  def toProgram(repr: SoftwareEntityRepr, programData: JavaProgramRepr): JavaProgram = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    new JavaProgram(repr.name, repr.name, repr.fqn, repr.repository, programData._2, hashedBytes)
  }

  def toPackage(repr: SoftwareEntityRepr): JavaPackage = new JavaPackage(repr.name, repr.fqn, repr.repository)

  def toClass(repr: SoftwareEntityRepr, classData: JavaClassRepr, interfaces: Set[String], nameLookup: Map[Long, String]): JavaClass = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    new JavaClass(repr.name, nameLookup(classData.typeNameId), repr.fqn, classData.superTypeNameId.map(nameLookup), interfaces,
      classData.isInterface, classData.isFinal, classData.isAbstract, repr.repository, hashedBytes)
  }

  def toMethod(repr: SoftwareEntityRepr, methodData: JavaMethodRepr, descriptorLookup: Map[Long, String]): JavaMethod = {

    new JavaMethod(repr.name, descriptorLookup(methodData.descriptorId), repr.fqn, methodData.isFinal, methodData.isStatic,
      methodData.isAbstract, methodData.visibility, repr.repository, methodData.hash)
  }

  def toInvocation(repr: SoftwareEntityRepr, invokeData: JavaInvocationRepr, nameLookup: Map[Long, String], descriptorLookup: Map[Long, String]): JavaInvokeStatement = {
    val invocationType = JavaInvocationType.fromId(invokeData.kindId)

    new JavaInvokeStatement(repr.name, nameLookup(invokeData.declTypeNameId), descriptorLookup(invokeData.descriptorId),
      invocationType, invokeData.pc, repr.fqn, repr.repository)
  }

  def toFieldAccess(repr: SoftwareEntityRepr, fieldAccessData: JavaFieldAccessRepr, nameLookup: Map[Long, String]): JavaFieldAccessStatement = {
    val accessType = JavaFieldAccessType.fromId(fieldAccessData.kindId)

    new JavaFieldAccessStatement(repr.name, nameLookup(fieldAccessData.fieldTypeNameId), nameLookup(fieldAccessData.declTypeNameId),
      accessType, fieldAccessData.pc, repr.fqn, repr.repository)
  }

  def toNewInstanceCreation(repr: SoftwareEntityRepr): JavaNewInstanceStatement = {
    val lastIndex = repr.fqn.lastIndexOf("!")

    if(lastIndex < 0 || lastIndex >= repr.fqn.length - 1)
      throw new IllegalStateException(s"Malformed uid for statement: ${repr.fqn}")

    val pc = repr.fqn.substring(lastIndex + 1).toInt
    new JavaNewInstanceStatement(repr.name, pc, repr.fqn, repr.repository)
  }
}
