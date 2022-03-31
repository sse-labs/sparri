package de.tudo.sse.classfilefeatures.webapi.storage.impl

import de.tudo.sse.classfilefeatures.common.model.InvocationTypes.InvocationType
import de.tudo.sse.classfilefeatures.common.model.{FieldAccessInstructionRepresentation, FieldAccessTypes, InvocationInstructionRepresentation, InvocationTypes, MethodBodyRepresentation, MethodRepresentation}

private[impl] object PostgreSqlStorageModel {

  class ClassFileEntry (val dbId: Int,
                        val thisType: String,
                        defaultFlags: Int,
                        defaultMajorVersion: Int,
                        defaultMinorVersion: Int,
                        defaultSuperType: Option[String]){

    var superTypeOpt: Option[String] = defaultSuperType

    var flags: Int = defaultFlags

    var majorVersion: Int = defaultMajorVersion

    var minorVersion: Int = defaultMinorVersion

    var interfaceTypes: Array[String] = Array.empty

  }

  class FieldDefinitionEntry (val dbId: Int,
                              val fieldName: String,
                              val fieldType: String,
                              defaultFlags: Int){
    var flags: Int = defaultFlags
  }

  class MethodDefinitionEntry (val dbId: Int,
                              val name: String,
                              val descriptor: String,
                              val hasBody: Boolean,
                              defaultFlags: Int,
                              defaultMaxStack: Option[Int],
                              defaultMaxLocals: Option[Int]) {

    var flags: Int = defaultFlags
    var maxStack: Option[Int] = defaultMaxStack
    var maxLocals: Option[Int] = defaultMaxLocals

    var invocationInstructions: Array[InvocationInstructionEntry] = Array.empty
    var fieldAccessInstructions: Array[FieldAccessInstructionEntry] = Array.empty

    def toModel: MethodRepresentation = {

      val bodyOption: Option[MethodBodyRepresentation] = if(hasBody) {

        Some(MethodBodyRepresentation(maxStack.get, maxLocals.get, invocationInstructions.map(_.toModel),
          fieldAccessInstructions.map(_.toModel)))
      } else {
        None
      }

      MethodRepresentation(flags, name, descriptor, bodyOption)
    }
  }

  class InvocationInstructionEntry (val dbId: Int,
                                    val methodName: String,
                                    val methodDescriptor: String,
                                    val declaredClass: String,
                                    val isInterfaceInvocation: Boolean,
                                    val invocationType: String){

    def toModel: InvocationInstructionRepresentation = InvocationInstructionRepresentation(methodName, methodDescriptor,
      declaredClass, isInterfaceInvocation, InvocationTypes.withName(invocationType))

  }

  class FieldAccessInstructionEntry (val dbId: Int,
                                     val fieldName: String,
                                     val fieldType: String,
                                     val declaredClass: String,
                                     val accessType: String){
    def toModel: FieldAccessInstructionRepresentation = FieldAccessInstructionRepresentation(fieldName, fieldType, declaredClass,
      FieldAccessTypes.withName(accessType))
  }

}
