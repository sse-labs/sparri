package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.execution.analyses.impl.ifds.TaintVariableFacts.TaintVariable

trait IFDSFact {
  def toString: String

  def uniqueIdent: String

  def displayName: String

  def isTaintVariable: Boolean = false

  def asTaintVariable: TaintVariable = throw new IllegalStateException("Not a taint variable")

  override def hashCode(): Int = uniqueIdent.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case f: IFDSFact => f.uniqueIdent.equals(uniqueIdent)
    case _ => false
  }
}


