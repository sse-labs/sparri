package org.anon.spareuse.execution.analyses.impl

package object cg {

  object OracleCallGraphResolutionMode extends Enumeration {
    final val CHA: Value = Value(0)
    final val NaiveRTA: Value = Value(1)
    final val RTA: Value = Value(2)

    type OracleCallGraphResolutionMode = Value

    def fromId(id: Int): OracleCallGraphResolutionMode = id match {
      case 0 => CHA
      case 1 => NaiveRTA
      case 2 => RTA
      case _ => throw new IllegalArgumentException(s"Invalid id for OracleResolutionMode $id")
    }
  }

}
