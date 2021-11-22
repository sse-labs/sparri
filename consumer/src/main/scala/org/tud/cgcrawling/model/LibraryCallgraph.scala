package org.tud.cgcrawling.model

import org.opalj.br.Method
import org.tud.cgcrawling.discovery.maven.MavenIdentifier

import scala.collection.mutable

class LibraryCallgraph {
  private val methods: mutable.Set[LibraryMethod] = new mutable.HashSet()
  private val invocationMap: mutable.Map[String, mutable.Set[LibraryMethod]] = new mutable.HashMap()

  def allMethods: Iterable[LibraryMethod] = methods

  def calleesOf(method: LibraryMethod): Iterable[LibraryMethod] = {
    invocationMap.getOrElse(method.identifier.fullSignature, throw new IllegalStateException("Method not part of CG: " + method.identifier.fullSignature))
  }

  def addMethod(method: Method, isExternal: Boolean, definingArtifact: Option[MavenIdentifier]): Unit = {
    val newMethod = new LibraryMethod(MethodIdentifier.fromOpalMethod(method, isExternal, definingArtifact))

    // This is still fast, because we use the fully signature for hashing
    if(!methods.contains(newMethod)){
      methods.add(newMethod)
      invocationMap.put(newMethod.identifier.fullSignature, mutable.HashSet.empty)
    }
  }

  def addEdge(caller: Method, callee: Method, calleeIsExternal: Boolean, definingArtifact: Option[MavenIdentifier]): Unit = {
    // We assume that the caller has already been processed with 'addMethod'!
    val calleeSet = invocationMap(caller.fullyQualifiedSignature)
    val newCallee = new LibraryMethod(MethodIdentifier.fromOpalMethod(callee, calleeIsExternal, definingArtifact))

    if(!calleeSet.contains(newCallee)){
      calleeSet.add(newCallee)
    }

  }

  def numberOfReachableMethods(): Int = methods.size
}

class LibraryMethod(val identifier: MethodIdentifier) {
  // Library Methods inside a single callgraph can be uniquely identified via their signature,
  // since we are loading the entire Classpath.
  override def hashCode(): Int = identifier.fullSignature.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case other: LibraryMethod => other.identifier.fullSignature.equals(identifier.fullSignature)
    case _ => false
  }
}
