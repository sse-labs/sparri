package de.tudo.sse.spareuse.core.model

object SoftwareEntityKind extends Enumeration {

  type SoftwareEntityKind = Value

  val Library: Value = Value(0)
  val Program: Value = Value(1)
  val Package: Value = Value(2)
  val Class: Value = Value(3)
  val Method: Value = Value(4)
  val Statement: Value = Value(5)


}
