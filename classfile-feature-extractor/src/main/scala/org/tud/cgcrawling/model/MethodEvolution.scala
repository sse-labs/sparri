package org.tud.cgcrawling.model

import de.tudo.classfilefeatures.common.maven.model.MavenIdentifier
import org.opalj.br.Method

import scala.collection.mutable

class MethodEvolution(val identifier: MethodIdentifier) extends CgElementEvolution {

  private val obligationMap: mutable.Map[InvocationObligation, InvocationObligationEvolution] = new mutable.HashMap()

  def obligationEvolutions(): Iterable[InvocationObligationEvolution] = obligationMap.values

  def setObligationActiveIn(obligation: InvocationObligation, version: String): Unit = {

    if(!obligationMap.contains(obligation)){
      obligationMap.put(obligation, new InvocationObligationEvolution(obligation))
    }

    obligationMap(obligation).addActiveRelease(version)
  }

}

class MethodIdentifier(val simpleName: String,
                       val methodDescriptor: String,
                       val typeFqn: String,
                       val fullSignature: String,
                       val isExternal: Boolean,
                       val isPublic: Boolean,
                       val definingArtifact: Option[MavenIdentifier]) {

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

  def definingLibraryName: String = {
    definingArtifact.map(i => s"${i.groupId}:${i.artifactId}").getOrElse{
      if(isJREMethod) "<none>:<jre>"
      else "<unknown>:<unknown>"
    }
  }
}

class InvocationObligationEvolution(val invocationObligation: InvocationObligation) extends CgElementEvolution

class InvocationObligation(val declaredTypeName: String, val methodName: String, val descriptor: String, isInterface: Boolean) {

  override def equals(obj: Any): Boolean = obj match {
    case o: InvocationObligation =>
      o.declaredTypeName.equals(declaredTypeName) && o.methodName.equals(methodName) && o.descriptor.equals(descriptor)
    case _ => false
  }

  override def hashCode(): Int = 23 * declaredTypeName.hashCode + 17 * methodName.hashCode + 13 * descriptor.hashCode
}

object MethodIdentifier {

  def fromOpalMethod(m: Method, isExternal: Boolean, definingArtifact: Option[MavenIdentifier]): MethodIdentifier = {
    new MethodIdentifier(m.name,
      m.descriptor.valueToString,
      m.classFile.thisType.fqn,
      m.fullyQualifiedSignature,
      isExternal,
      m.isPublic,
      definingArtifact)
  }

}