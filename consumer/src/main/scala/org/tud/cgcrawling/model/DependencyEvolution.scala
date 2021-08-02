package org.tud.cgcrawling.model

import org.tud.cgcrawling.discovery.maven.MavenIdentifier

class DependencyEvolution(val identifier: DependencyIdentifier) extends CgElementEvolution

class DependencyIdentifier(val identifier: MavenIdentifier, val scope: String = "default") {

  override def equals(obj: Any): Boolean =  obj match {
    case other: DependencyIdentifier => other.identifier.equals(identifier) && other.scope.equals(scope)
    case _ => false
  }

  override def hashCode(): Int = {
    41 * identifier.hashCode() + 13 * scope.hashCode
  }

}
