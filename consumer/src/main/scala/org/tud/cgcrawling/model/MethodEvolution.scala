package org.tud.cgcrawling.model

import org.opalj.br.Method
import org.tud.cgcrawling.model.MethodIdentifier.{jrePackages}

class MethodEvolution(val identifier: MethodIdentifier) extends CgElementEvolution

class MethodIdentifier(val simpleName: String, val packageName: String, val fullSignature: String, val isExternal: Boolean, val isPublic: Boolean) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case identifier: MethodIdentifier =>
        identifier.fullSignature.equals(fullSignature) &&
          identifier.isExternal == isExternal &&
          identifier.isPublic == isPublic
      case _ =>
        false
    }
  }

  override def hashCode(): Int = fullSignature.hashCode + 5 * isExternal.hashCode() + 3 * isPublic.hashCode()

  def isJREMethod: Boolean = {
    jrePackages.exists(packageName.startsWith)
  }
}

object MethodIdentifier {

  private val jrePackages = Set(
    "com/sun", "sun", "oracle", "jdk", "java", "com/oracle", "javax", "sunw"
  )

  def fromOpalMethod(m: Method, isExternal: Boolean): MethodIdentifier = {
    new MethodIdentifier(m.name, m.classFile.thisType.packageName, m.fullyQualifiedSignature, isExternal, m.isPublic)
  }

}