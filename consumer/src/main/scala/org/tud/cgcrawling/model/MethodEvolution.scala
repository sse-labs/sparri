package org.tud.cgcrawling.model

import org.opalj.br.Method
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.opal.OPALProjectHelper

class MethodEvolution(val identifier: MethodIdentifier) extends CgElementEvolution

class MethodIdentifier(val simpleName: String, val packageName: String, val fullSignature: String, val isExternal: Boolean, val isPublic: Boolean, val definingArtifact: Option[MavenIdentifier]) {

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

  def isJREMethod: Boolean = isExternal && definingArtifact.isEmpty
}

object MethodIdentifier {

  def fromOpalMethod(m: Method, isExternal: Boolean, definingArtifact: Option[MavenIdentifier]): MethodIdentifier = {
    new MethodIdentifier(m.name, m.classFile.thisType.packageName, m.fullyQualifiedSignature, isExternal, m.isPublic, definingArtifact)
  }

}