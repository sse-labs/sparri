package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaFieldAccessType, JavaInvocationType, JavaInvokeStatement, JavaLibrary, JavaMethod, JavaNewInstanceStatement, JavaPackage, JavaProgram}
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaFieldAccessRepr, JavaInvocationRepr, JavaMethodRepr, JavaProgramRepr}
import org.anon.spareuse.core.utils.fromHex

object JavaConverter {



  def toLib(repr: SoftwareEntityRepr): JavaLibrary = {
    val jl = new JavaLibrary(repr.name, repr.repository, repr.id)
    jl.setDataBaseId(repr.id)
    jl
  }

  def toProgram(repr: SoftwareEntityRepr, programData: JavaProgramRepr): JavaProgram = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    val jp = new JavaProgram(repr.name, repr.identifier, repr.id, repr.repository, programData._2, hashedBytes)
    jp.setDataBaseId(repr.id)
    jp
  }

  def toPackage(repr: SoftwareEntityRepr): JavaPackage = {
    val jp = new JavaPackage(repr.name, repr.id, repr.repository)
    jp.setDataBaseId(repr.id)
    jp
  }

  def toClass(repr: SoftwareEntityRepr, classData: JavaClassRepr, interfaces: Set[String], nameLookup: Map[Long, String]): JavaClass = {
    val hashedBytes: Array[Byte] = repr.hexHash.map(fromHex).getOrElse(Array.empty)
    val jc = new JavaClass(repr.name, nameLookup(classData.typeNameId), repr.id, classData.superTypeNameId.map(nameLookup), interfaces,
      classData.isInterface, classData.isFinal, classData.isAbstract, repr.repository, hashedBytes)
    jc.setDataBaseId(repr.id)
    jc
  }

  def toMethod(repr: SoftwareEntityRepr, methodData: JavaMethodRepr, descriptorLookup: Map[Long, String]): JavaMethod = {
    val jm = new JavaMethod(repr.name, descriptorLookup(methodData.descriptorId), repr.id, methodData.isFinal, methodData.isStatic,
      methodData.isAbstract, methodData.visibility, repr.repository, methodData.hash)
    jm.setDataBaseId(repr.id)
    jm
  }

  def toInvocation(repr: SoftwareEntityRepr, invokeData: JavaInvocationRepr, nameLookup: Map[Long, String], descriptorLookup: Map[Long, String]): JavaInvokeStatement = {
    val invocationType = JavaInvocationType.fromId(invokeData.kindId)

    val jis = new JavaInvokeStatement(repr.name, nameLookup(invokeData.declTypeNameId), descriptorLookup(invokeData.descriptorId),
      invocationType, invokeData.pc, repr.id, repr.repository)
    jis.setDataBaseId(repr.id)
    jis
  }

  def toFieldAccess(repr: SoftwareEntityRepr, fieldAccessData: JavaFieldAccessRepr, nameLookup: Map[Long, String]): JavaFieldAccessStatement = {
    val accessType = JavaFieldAccessType.fromId(fieldAccessData.kindId)

    val jfa = new JavaFieldAccessStatement(repr.name, nameLookup(fieldAccessData.fieldTypeNameId), nameLookup(fieldAccessData.declTypeNameId),
      accessType, fieldAccessData.pc, repr.id, repr.repository)
    jfa.setDataBaseId(repr.id)
    jfa
  }

  def toNewInstanceCreation(repr: SoftwareEntityRepr): JavaNewInstanceStatement = {
    val pc = repr.identifier.toInt
    val jnis = new JavaNewInstanceStatement(repr.name, pc, repr.id, repr.repository)
    jnis.setDataBaseId(repr.id)
    jnis
  }
}
