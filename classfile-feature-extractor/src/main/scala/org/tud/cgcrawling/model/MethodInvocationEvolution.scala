package org.tud.cgcrawling.model

class MethodInvocationEvolution(val invocationIdent: MethodInvocationIdentifier) extends CgElementEvolution

class MethodInvocationIdentifier(val callerIdent: MethodIdentifier, val calleeIdent: MethodIdentifier){

  override def equals(obj: Any): Boolean = obj match {
    case i: MethodInvocationIdentifier =>
      i.callerIdent.equals(callerIdent) && i.calleeIdent.equals(calleeIdent)
    case _ => false
  }

  override def hashCode(): Int = 11 * callerIdent.hashCode() + 5 * calleeIdent.hashCode()
}
