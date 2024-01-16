package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaNewInstanceStatement}
import spray.json.{DefaultJsonProtocol, JsonFormat}

case class JreRepresentation(version: String, types: Seq[JreType]){
  def allTypesInstantiated: Set[String] = types.flatMap(_.allTypesInstantiated).toSet
}
case class JreType(t: String, s: Option[String], i: Seq[String], iI: Boolean, m: Seq[JreMethod]){
  def allTypesInstantiated: Set[String]= m.flatMap(_.allTypesInstantiated).toSet

  lazy val asModel: JavaClass = {
    val jc = new JavaClass(t, t, "<none>", s, i.toSet, iI, finalType = false, abstractType = false, "<default>", hashedBytes = Array.empty)
    m.foreach{ jreM => jc.addChild(jreM.asModel)}
    jc
  }
}

/**
 * Represents a method
 * @param n Method name
 * @param d Method descriptor
 * @param t new instantiations
 * @param i invocations
 * @param iA isAbstract flag
 * @param v visibility
 */
case class JreMethod(n: String, d: String, t: Seq[JreNew], i: Seq[JreInvoke], iA: Boolean, v: String){
  def allTypesInstantiated: Set[String] = t.map(_.t).toSet

  def asModel: JavaMethod = {
    val jm = new JavaMethod(n, d, "<none>", finalMethod = false, staticMethod = false, abstractMethod = false, "<none>", "<default>", -1)

    t.foreach(jreNew => jm.addChild(jreNew.asModel))
    i.foreach(jreInvoke => jm.addChild(jreInvoke.asModel))

    jm
  }
}

/**
 * Represents a new instance creation
 * @param t type that is instantiated
 * @param p Pc of instruction
 */
case class JreNew(t: String, p: Long){

  def asModel: JavaNewInstanceStatement =
    new JavaNewInstanceStatement(t, p.toInt, "<none>", "<default>")

}

/**
 * Represents an invocation
 * @param t Declared type
 * @param n Method name
 * @param d Method descriptor
 * @param p PC
 * @param k Kind of the invocation, as int
 */
case class JreInvoke(t: String, n: String, d: String, p: Long, k: Int){

  def asModel: JavaInvokeStatement =
    new JavaInvokeStatement(n, t, d, JavaInvocationType.fromId(k), p.toInt, "<none>", "<default>")

}

trait JreRepresentationJsonSupport extends DefaultJsonProtocol {
  implicit val invokeJsonFormat: JsonFormat[JreInvoke] = jsonFormat5(JreInvoke)
  implicit val newJsonFormat: JsonFormat[JreNew] = jsonFormat2(JreNew)
  implicit val methodJsonFormat: JsonFormat[JreMethod] = jsonFormat6(JreMethod)
  implicit val typeJsonFormat: JsonFormat[JreType] = jsonFormat(JreType, "t", "s", "i", "iI", "m")
  implicit val jreJsonFormat: JsonFormat[JreRepresentation] = jsonFormat2(JreRepresentation)
}