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

  def toClass(repr: SoftwareEntityRepr, classData: JavaClassRepr): JavaClass = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    val interfaces = if(classData._4.isBlank) Set.empty[String]
      else classData._4.split(";").toSet
    new JavaClass(repr.name, classData._2, repr.fqn, classData._3, interfaces, classData._5, classData._6, classData._7, repr.repository, hashedBytes)
  }

  def toMethod(repr: SoftwareEntityRepr, methodData: JavaMethodRepr): JavaMethod = {

    new JavaMethod(repr.name, methodData._2, repr.fqn, methodData._3, methodData._4, methodData._5, methodData._6, repr.repository, methodData._7)
  }

  def toInvocation(repr: SoftwareEntityRepr, invokeData: JavaInvocationRepr): JavaInvokeStatement = {
    val invocationType = JavaInvocationType.fromId(invokeData._4)

    new JavaInvokeStatement(repr.name, invokeData._2, invokeData._3, invocationType, invokeData._5, repr.fqn, repr.repository)
  }

  def toFieldAccess(repr: SoftwareEntityRepr, fieldAccessData: JavaFieldAccessRepr): JavaFieldAccessStatement = {
    val accessType = JavaFieldAccessType.fromId(fieldAccessData._4)

    new JavaFieldAccessStatement(repr.name, fieldAccessData._2, fieldAccessData._3, accessType, fieldAccessData._5, repr.fqn, repr.repository)
  }

  def toNewInstanceCreation(repr: SoftwareEntityRepr): JavaNewInstanceStatement = {
    val lastIndex = repr.fqn.lastIndexOf("!")

    if(lastIndex < 0 || lastIndex >= repr.fqn.length - 1)
      throw new IllegalStateException(s"Malformed uid for statement: ${repr.fqn}")

    val pc = repr.fqn.substring(lastIndex + 1).toInt
    new JavaNewInstanceStatement(repr.name, pc, repr.fqn, repr.repository)
  }
}
