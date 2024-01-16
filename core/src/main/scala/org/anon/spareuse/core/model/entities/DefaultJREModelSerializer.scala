package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaNewInstanceStatement, JavaPackage, JavaProgram, JavaStatement}
import org.anon.spareuse.core.utils.toHex
import spray.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString}

object DefaultJREModelSerializer extends JREModelSerializer {

  protected[entities] override def modelToJson(model: JavaProgram): JsObject = {
    def stmtToJson(stmt: JavaStatement): JsObject = stmt match {
      case jis: JavaInvokeStatement =>
        new JsObject(Map(
          "methodName" -> JsString(jis.targetMethodName),
          "declaredTypeFqn" -> JsString(jis.targetTypeName),
          "descriptor" -> JsString(jis.targetDescriptor),
          "invocationType" -> JsNumber(jis.invokeStatementType.id),
          "pc" -> JsNumber(jis.instructionPc),
          "uid" -> JsString(jis.uid)
        ))
      case jfas: JavaFieldAccessStatement =>
        new JsObject(Map(
          "fieldName" -> JsString(jfas.targetFieldName),
          "fieldTypeFqn" -> JsString(jfas.targetFieldTypeName),
          "declaredTypeFqn" -> JsString(jfas.targetTypeName),
          "accessType" -> JsNumber(jfas.fieldAccessType.id),
          "pc" -> JsNumber(jfas.instructionPc),
          "uid" -> JsString(jfas.uid)
        ))
      case jnis: JavaNewInstanceStatement =>
        new JsObject(Map(
          "typeName" -> JsString(jnis.instantiatedTypeName),
          "pc" -> JsNumber(jnis.instructionPc),
          "uid" -> JsString(jnis.uid)
        ))
    }

    def methodToJson(method: JavaMethod): JsObject = new JsObject(Map(
      "methodName" -> JsString(method.name),
      "descriptor" -> JsString(method.descriptor),
      "methodUid" -> JsString(method.uid),
      "finalMethod" -> JsBoolean(method.isFinal),
      "staticMethod" -> JsBoolean(method.isStatic),
      "abstractMethod" -> JsBoolean(method.isAbstract),
      "methodVisibility" -> JsString(method.visibility),
      "hash" -> JsNumber(method.methodHash),
      "statements" -> JsArray(method.getStatements.map(stmtToJson).toVector)
    ))

    def classToJson(klass: JavaClass): JsObject = new JsObject(Map(
      "className" -> JsString(klass.name),
      "thisTypeFqn" -> JsString(klass.thisType),
      "classUid" -> JsString(klass.uid),
      "superTypeFqn" -> JsString(klass.superType.getOrElse("NONE")),
      "interfaceFqns" -> JsArray(klass.interfaceTypes.map(n => JsString(n)).toVector),
      "interfaceType" -> JsBoolean(klass.isInterface),
      "finalType" -> JsBoolean(klass.isFinal),
      "abstractType" -> JsBoolean(klass.isAbstract),
      "hashedBytes" -> JsString(klass.binaryHash.map(toHex).getOrElse("NONE")),
      "methods" -> JsArray(klass.getMethods.map(methodToJson).toVector)
    ))

    def packageToJson(p: JavaPackage): JsObject = new JsObject(Map(
      "packageName" -> JsString(p.name),
      "packageUid" -> JsString(p.uid),
      "classes" -> JsArray(p.getClasses.map(classToJson).toVector)
    ))

    def programToJson(cp: JavaProgram): JsObject = new JsObject(Map(
      "programName" -> JsString(cp.name),
      "programIdent" -> JsString(cp.identifier),
      "programUid" -> JsString(cp.uid),
      "repositoryIdent" -> JsString(cp.repository),
      "hashedBytes" -> JsString(cp.binaryHash.map(toHex).getOrElse("NONE")),
      "packages" -> JsArray(cp.getPackages.map(packageToJson).toVector)
    ))

    programToJson(model)
  }

}
