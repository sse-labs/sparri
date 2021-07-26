package org.tud.cgcrawling.model

import org.opalj.br.DeclaredMethod

class MethodEvolution(val identifier: MethodIdentifier) extends CgElementEvolution

class MethodIdentifier(val simpleName: String, val fullSignature: String, val isExternal: Boolean) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case identifier: MethodIdentifier =>
        identifier.fullSignature.equals(fullSignature) && identifier.isExternal == isExternal
      case _ =>
        false
    }
  }

  override def hashCode(): Int = fullSignature.hashCode + 5 * isExternal.hashCode()
}

object MethodIdentifier {
  def fromOpalMethod(m: DeclaredMethod, isExternal: Boolean) = new MethodIdentifier(m.name, m.toJava, isExternal)
}