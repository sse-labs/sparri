package org.anon.spareuse.execution.analyses.impl.ifds

trait IFDSFact {
  def toString: String

  def uniqueIdent: String

  def displayName: String

  override def hashCode(): Int = uniqueIdent.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case f: IFDSFact => f.uniqueIdent.equals(uniqueIdent)
    case _ => false
  }
}


