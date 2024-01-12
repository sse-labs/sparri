package org.anon.spareuse.execution.analyses.impl.cg

import spray.json.{DefaultJsonProtocol, JsonFormat}

case class JreRepresentation(version: String, types: Seq[JreType]){
  def allTypesInstantiated: Set[String] = types.flatMap(_.allTypesInstantiated).toSet
}
case class JreType(t: String, s: Option[String], i: Seq[String], iI: Boolean, m: Seq[JreMethod]){
  def allTypesInstantiated: Set[String]= m.flatMap(_.allTypesInstantiated).toSet
}

/**
 * Represents a method
 * @param n Method name
 * @param d Method descriptor
 * @param t new instantiations
 * @param i invocations
 */
case class JreMethod(n: String, d: String, t: Seq[JreNew], i: Seq[JreInvoke]){
  def allTypesInstantiated: Set[String] = t.map(_.t).toSet
}

/**
 * Represents a new instance creation
 * @param t type that is instantiated
 * @param p Pc of instruction
 */
case class JreNew(t: String, p: Long)

/**
 * Represents an invocation
 * @param t Declared type
 * @param n Method name
 * @param d Method descriptor
 * @param p PC
 * @param k Kind of the invocation, as int
 */
case class JreInvoke(t: String, n: String, d: String, p: Long, k: Int)

trait JreRepresentationJsonSupport extends DefaultJsonProtocol {
  implicit val invokeJsonFormat: JsonFormat[JreInvoke] = jsonFormat5(JreInvoke)
  implicit val newJsonFormat: JsonFormat[JreNew] = jsonFormat2(JreNew)
  implicit val methodJsonFormat: JsonFormat[JreMethod] = jsonFormat4(JreMethod)
  implicit val typeJsonFormat: JsonFormat[JreType] = jsonFormat5(JreType)
  implicit val jreJsonFormat: JsonFormat[JreRepresentation] = jsonFormat2(JreRepresentation)
}