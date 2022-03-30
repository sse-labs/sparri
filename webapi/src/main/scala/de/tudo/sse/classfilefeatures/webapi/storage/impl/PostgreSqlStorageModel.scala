package de.tudo.sse.classfilefeatures.webapi.storage.impl

import de.tudo.sse.classfilefeatures.common.model.{MethodBodyRepresentation, MethodRepresentation}

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

    def toModel: MethodRepresentation = {

      val bodyOption: Option[MethodBodyRepresentation] = if(hasBody) {
        //TODO: Instructions
        Some(MethodBodyRepresentation(maxStack.get, maxLocals.get, Seq.empty, Seq.empty))
      } else {
        None
      }

      MethodRepresentation(flags, name, descriptor, bodyOption)
    }
  }

}
