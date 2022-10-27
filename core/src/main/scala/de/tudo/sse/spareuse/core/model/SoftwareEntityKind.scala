package de.tudo.sse.spareuse.core.model

object SoftwareEntityKind extends Enumeration {

  type SoftwareEntityKind = Value

  val Library: Value = Value(0)
  val Program: Value = Value(1)
  val Package: Value = Value(2)
  val Class: Value = Value(3)
  val Method: Value = Value(4)
  val InvocationStatement: Value = Value(5)
  val FieldAccessStatement: Value = Value(6)

  def isLessSpecificOrEqual(toTest: SoftwareEntityKind, pivot: SoftwareEntityKind): Boolean = {
    if(pivot.id < 5) toTest.id <= pivot.id
    else true
  }

  def isLessSpecific(toTest: SoftwareEntityKind, pivot: SoftwareEntityKind): Boolean = {
    if(pivot.id <= 5) toTest.id < pivot.id
    else toTest.id < 5
  }
}
