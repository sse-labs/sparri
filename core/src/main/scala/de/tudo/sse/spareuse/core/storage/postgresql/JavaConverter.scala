package de.tudo.sse.spareuse.core.storage.postgresql

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaFieldAccessType, JavaInvocationType, JavaInvokeStatement, JavaLibrary, JavaMethod, JavaPackage, JavaProgram}
import de.tudo.sse.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaFieldAccessRepr, JavaInvocationRepr, JavaMethodRepr}
import de.tudo.sse.spareuse.core.utils.fromHex

object JavaConverter {



  def toLib(repr: SoftwareEntityRepr): JavaLibrary = new JavaLibrary(repr.name, repr.repository)

  def toProgram(repr: SoftwareEntityRepr): JavaProgram = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    new JavaProgram(repr.name, repr.name, repr.repository, hashedBytes)
  }

  def toPackage(repr: SoftwareEntityRepr): JavaPackage = new JavaPackage(repr.name, repr.repository)

  def toClass(repr: SoftwareEntityRepr, classData: JavaClassRepr): JavaClass = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    new JavaClass(repr.name, classData._2, classData._3, repr.repository, hashedBytes)
  }

  def toMethod(repr: SoftwareEntityRepr, methodData: JavaMethodRepr): JavaMethod = {
    val paramTypeNames = if(!methodData._4.trim.isBlank) methodData._4.split(",") else Array.empty[String]

    if (paramTypeNames.length != methodData._3)
      throw new IllegalStateException("Corrupt database, parameter count does not match actual parameters")

    new JavaMethod(repr.name, methodData._2, paramTypeNames.toSeq, repr.repository)
  }

  def toInvocation(repr: SoftwareEntityRepr, invokeData: JavaInvocationRepr): JavaInvokeStatement = {
    val invocationType = JavaInvocationType.fromId(invokeData._5)

    new JavaInvokeStatement(repr.name, invokeData._2, invokeData._3, invokeData._4, invocationType, invokeData._6, repr.repository)
  }

  def toFieldAccess(repr: SoftwareEntityRepr, fieldAccessData: JavaFieldAccessRepr): JavaFieldAccessStatement = {
    val accessType = JavaFieldAccessType.fromId(fieldAccessData._4)

    new JavaFieldAccessStatement(repr.name, fieldAccessData._2, fieldAccessData._3, accessType, fieldAccessData._5, repr.repository)
  }
}
