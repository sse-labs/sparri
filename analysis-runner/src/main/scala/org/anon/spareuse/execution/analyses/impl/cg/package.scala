package org.anon.spareuse.execution.analyses.impl

package object cg {

  object OracleCallGraphResolutionMode extends Enumeration {
    final val CHA: Value = Value("CHA")
    final val NaiveRTA: Value = Value("NaiveRTA")
    final val RTA: Value = Value("RTA")

    type OracleCallGraphResolutionMode = Value
  }

}
