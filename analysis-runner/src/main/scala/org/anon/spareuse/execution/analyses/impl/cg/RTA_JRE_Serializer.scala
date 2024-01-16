package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.{JREModelSerializer, JavaEntities}
import spray.json.{JsObject, enrichAny}

object RTA_JRE_Serializer extends JREModelSerializer with JreRepresentationJsonSupport {
  
  override def modelToJson(model: JavaEntities.JavaProgram): JsObject = {

    val repr = JreRepresentation(model.v, model.allClasses.map { jc =>
      JreType(jc.thisType, jc.superType, jc.interfaceTypes.toSeq, jc.isInterface, jc.getMethods.map {jm =>
        JreMethod(jm.name, jm.descriptor, jm.getNewStatements.map(jnis => JreNew(jnis.instantiatedTypeName, jnis.instructionPc)), jm.getInvocationStatements.map{jis =>
          JreInvoke(jis.targetTypeName, jis.targetMethodName, jis.targetDescriptor, jis.instructionPc, jis.invokeStatementType.id
          )
        }, jm.isAbstract, jm.visibility)
      }.toSeq)
    }.toSeq)

    repr.toJson.asJsObject
  }
}
