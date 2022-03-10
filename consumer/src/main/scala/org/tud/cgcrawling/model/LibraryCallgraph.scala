package org.tud.cgcrawling.model

import org.opalj.br.Method
import org.tud.cgcrawling.discovery.maven.MavenIdentifier

import scala.collection.mutable

class LibraryCallgraph(val instantiatedTypeNames: Set[String], val hierarchy: ClassHierarchy) {

  private val methodsMap: mutable.Map[String, LibraryMethod] = new mutable.HashMap()
  private val invocationMap: mutable.Map[String, mutable.Set[LibraryMethod]] = new mutable.HashMap()

  def allMethods: Iterable[LibraryMethod] = methodsMap.values

  def calleesOf(method: LibraryMethod): Iterable[LibraryMethod] = {
    invocationMap.getOrElse(method.identifier.fullSignature, throw new IllegalStateException("Method not part of CG: " + method.identifier.fullSignature))
  }

  def addMethod(method: Method, isExternal: Boolean, definingArtifact: Option[MavenIdentifier],
                obligations: Array[InvocationObligation]): Unit = {
    if(!methodsMap.contains(method.fullyQualifiedSignature)){
      val newMethod = new LibraryMethod(MethodIdentifier.fromOpalMethod(method, isExternal, definingArtifact), obligations)
      methodsMap.put(method.fullyQualifiedSignature, newMethod)
      invocationMap.put(method.fullyQualifiedSignature, mutable.HashSet.empty)
    } else {
      // This method has been added as a callee already, so just update obligations
      methodsMap.update(method.fullyQualifiedSignature,
        methodsMap(method.fullyQualifiedSignature).withObligations(obligations))
    }
  }

  def addEdge(caller: Method, callee: Method, calleeIsExternal: Boolean, definingArtifact: Option[MavenIdentifier]): Unit = {
    // We assume that the caller has already been processed with 'addMethod'!
    val calleeSet = invocationMap(caller.fullyQualifiedSignature)

    val currentCallee = if(!methodsMap.contains(callee.fullyQualifiedSignature)){
      val c = new LibraryMethod(MethodIdentifier.fromOpalMethod(callee, calleeIsExternal, definingArtifact))
      methodsMap.put(callee.fullyQualifiedSignature, c)
      invocationMap.put(callee.fullyQualifiedSignature, mutable.HashSet.empty)
      c
    } else {
      methodsMap(callee.fullyQualifiedSignature)
    }

    if(!calleeSet.contains(currentCallee)){
      calleeSet.add(currentCallee)
    }

  }

  def numberOfReachableMethods(): Int = methodsMap.size
}

class LibraryMethod(val identifier: MethodIdentifier, val obligations: Array[InvocationObligation] = Array.empty) {
  // Library Methods inside a single callgraph can be uniquely identified via their signature,
  // since we are loading the entire Classpath.
  override def hashCode(): Int = identifier.fullSignature.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case other: LibraryMethod => other.identifier.fullSignature.equals(identifier.fullSignature)
    case _ => false
  }

  def withObligations(obligations: Array[InvocationObligation]): LibraryMethod = {
    new LibraryMethod(identifier, obligations)
  }
}

