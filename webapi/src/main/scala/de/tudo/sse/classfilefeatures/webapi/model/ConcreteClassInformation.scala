package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import de.tudo.sse.classfilefeatures.common.model.{ClassFileRepresentation, FieldAccessInstructionRepresentation, FieldDefinitionRepresentation, InvocationInstructionRepresentation, MethodRepresentation}
import org.opalj.bi.{ACC_ENUM, ACC_FINAL, ACC_INTERFACE, ACC_MODULE, ACC_PRIVATE, ACC_PROTECTED, ACC_PUBLIC, ACC_STATIC, ACC_TRANSIENT, ACC_VOLATILE}
import org.opalj.br.ClassFile.{annotationMask, classCategoryMask}
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class ConcreteClassInformation (libraryName: String,
                                           releaseName: String,
                                           thisTypeFqn: String,
                                           flags: Int,
                                           accessInformation: ClassAccessInformation,
                                           cfMajorVersion: Int,
                                           cfMinorVersion: Int,
                                           superTypeFqn: Option[String],
                                           interfaceFqns: Array[String],
                                           fieldDefinitions: Array[FieldDefinitionInformation],
                                           methodsDefinitions: Array[MethodDefinitionInformation])

final case class ClassAccessInformation(isPublic: Boolean,
                                   isClass: Boolean,
                                   isEnum: Boolean,
                                   isModule: Boolean,
                                   isInterface: Boolean,
                                   isAnnotation: Boolean)


final case class FieldDefinitionInformation(fieldName: String,
                                            fieldTypeName: String,
                                            flags: Int,
                                            accessInformation: ClassMemberAccessInformation)

final case class MethodDefinitionInformation(methodName: String,
                                             methodDescriptor: String,
                                             flags: Int,
                                             accessInformation: ClassMemberAccessInformation,
                                             hasBody: Boolean,
                                             maxStack: Option[Int],
                                             maxLocals: Option[Int],
                                             invocations: Array[InvocationInformation],
                                             fieldAccesses: Array[FieldAccessInformation])

final case class InvocationInformation(methodName: String,
                                       methodDescriptor: String,
                                       declaredClass: String,
                                       invocationType: String)

final case class FieldAccessInformation(fieldName: String,
                                        fieldType: String,
                                        declaredClass: String,
                                        accessType: String)

final case class ClassMemberAccessInformation(isPublic: Boolean,
                                              isPrivate: Boolean,
                                              isProtected: Boolean,
                                              isStatic: Boolean,
                                              isFinal: Boolean,
                                              isTransient: Boolean,
                                              isVolatile: Boolean)

object ConcreteClassInformationBuilder {

  def fromRepresentation(libraryName: String, releaseName: String, cfr: ClassFileRepresentation): ConcreteClassInformation = {

    val isPublic = (ACC_PUBLIC.mask & cfr.flags) != 0
    val isClass = (cfr.flags & classCategoryMask) == 0
    val isEnum = (cfr.flags & ACC_ENUM.mask) == ACC_ENUM.mask
    val isModule = cfr.flags == ACC_MODULE.mask
    val isInterface = (cfr.flags & ACC_INTERFACE.mask) == ACC_INTERFACE.mask
    val isAnnotation = (cfr.flags & classCategoryMask) == annotationMask


    ConcreteClassInformation(
      libraryName,
      releaseName,
      cfr.thisTypeFqn,
      cfr.flags,
      ClassAccessInformation(isPublic, isClass, isEnum, isModule, isInterface, isAnnotation),
      cfr.majorVersion,
      cfr.minorVersion,
      cfr.superTypeFqn,
      cfr.interfacesFqn.toArray,
      cfr.fieldRepresentations.map(fromRepresentation).toArray,
      cfr.methodRepresentations.map(fromRepresentation).toArray)
  }

  def fromRepresentation(fdr: FieldDefinitionRepresentation): FieldDefinitionInformation = {

    FieldDefinitionInformation(
      fdr.name,
      fdr.fieldTypeJvmName,
      fdr.flags,
      buildClassMemberAccessInfo(fdr.flags)
    )
  }

  def fromRepresentation(mr: MethodRepresentation): MethodDefinitionInformation = {
    MethodDefinitionInformation(
      mr.name,
      mr.jvmMethodDescriptor,
      mr.flags,
      buildClassMemberAccessInfo(mr.flags),
      mr.body.isDefined,
      mr.body.map(_.maxStack),
      mr.body.map(_.maxLocals),
      mr.body.map(b => b.invocations.map(fromRepresentation)).getOrElse(Seq.empty).toArray,
      mr.body.map(b => b.fieldAccesses.map(fromRepresentation)).getOrElse(Seq.empty).toArray
    )
  }

  def fromRepresentation(iir: InvocationInstructionRepresentation): InvocationInformation = InvocationInformation(iir.targetMethodName,
    iir.targetMethodJvmDescriptor, iir.targetMethodDeclaredClassFqn, iir.invocationType.toString)

  def fromRepresentation(fair: FieldAccessInstructionRepresentation): FieldAccessInformation = FieldAccessInformation(fair.fieldName,
    fair.fieldTypeJvmName, fair.fieldDeclaredClassFqn, fair.fieldAccessType.toString)

  private def buildClassMemberAccessInfo(flags: Int): ClassMemberAccessInformation = {
    val isTransient = (ACC_TRANSIENT.mask & flags) != 0
    val isVolatile = (ACC_VOLATILE.mask & flags) != 0
    val isPublic = (ACC_PUBLIC.mask & flags) != 0
    val isStatic = (ACC_STATIC.mask & flags) != 0
    val isProtected = (ACC_PROTECTED.mask & flags) != 0
    val isPrivate = (ACC_PRIVATE.mask & flags) != 0
    val isFinal = (ACC_FINAL.mask & flags) != 0

    ClassMemberAccessInformation(isPublic, isPrivate, isProtected, isStatic, isFinal, isTransient, isVolatile)
  }
}

trait ConcreteClassInformationJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {

  implicit val classAccessInfoFormat: JsonFormat[ClassAccessInformation] = jsonFormat6(ClassAccessInformation)

  implicit val classMemberAccessInfoFormat: JsonFormat[ClassMemberAccessInformation] = jsonFormat7(ClassMemberAccessInformation)

  implicit val fieldDefinitionInfoFormat: JsonFormat[FieldDefinitionInformation] = jsonFormat4(FieldDefinitionInformation)

  implicit val invocationInfoFormat: JsonFormat[InvocationInformation] = jsonFormat4(InvocationInformation)

  implicit val fieldAccessInfoFormat: JsonFormat[FieldAccessInformation] = jsonFormat4(FieldAccessInformation)

  implicit val methodDefinitionInfoFormat: JsonFormat[MethodDefinitionInformation] = jsonFormat9(MethodDefinitionInformation)

  implicit val classInfoFormat: JsonFormat[ConcreteClassInformation] = jsonFormat11(ConcreteClassInformation)

}