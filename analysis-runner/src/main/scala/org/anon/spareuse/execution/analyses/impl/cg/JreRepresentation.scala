package org.anon.spareuse.execution.analyses.impl.cg

import spray.json.{DefaultJsonProtocol, JsonFormat}

case class JreRepresentation(version: String, types: Seq[JreType]){
  def allTypesInstantiated: Set[String] = types.flatMap(_.allTypesInstantiated).toSet
}
case class JreType(t: String, s: Option[String], i: Seq[String], iI: Boolean, m: Seq[JreMethod]){
  def allTypesInstantiated: Set[String]= m.flatMap(_.allTypesInstantiated).toSet
}
case class JreMethod(name: String, descr: String, n: Seq[JreNew], i: Seq[JreInvoke]){
  def allTypesInstantiated: Set[String] = n.map(_.t).toSet
}
case class JreNew(t: String, pc: Long)
case class JreInvoke(dT: String, n: String, descr: String, iS: Boolean, pc: Long)

trait JreRepresentationJsonSupport extends DefaultJsonProtocol {
  implicit val invokeJsonFormat: JsonFormat[JreInvoke] = jsonFormat5(JreInvoke)
  implicit val newJsonFormat: JsonFormat[JreNew] = jsonFormat2(JreNew)
  implicit val methodJsonFormat: JsonFormat[JreMethod] = jsonFormat4(JreMethod)
  implicit val typeJsonFormat: JsonFormat[JreType] = jsonFormat5(JreType)
  implicit val jreJsonFormat: JsonFormat[JreRepresentation] = jsonFormat2(JreRepresentation)
}