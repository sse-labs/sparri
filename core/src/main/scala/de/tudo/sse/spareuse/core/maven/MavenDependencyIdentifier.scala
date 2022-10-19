package de.tudo.sse.spareuse.core.maven

case class MavenDependencyIdentifier(identifier: MavenIdentifier, scope: String = "default") {

  override def equals(obj: Any): Boolean = obj match {
    case other: MavenDependencyIdentifier => other.identifier.equals(identifier) && other.scope.equals(scope)
    case _ => false
  }

  override def toString: String = identifier.toString + ":" + scope

  override def hashCode(): Int = {
    41 * identifier.hashCode() + 13 * scope.hashCode
  }

}
