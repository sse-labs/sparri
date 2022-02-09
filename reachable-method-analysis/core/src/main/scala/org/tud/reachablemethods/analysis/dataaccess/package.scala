package org.tud.reachablemethods.analysis

package object dataaccess {

  case class ElasticMethodData(name: String, signature: String, typeFqn: String, isExtern: Boolean, obligations: Iterable[InvocationObligation],
                               calleeSignatures: Iterable[String], definingLibrary: String, analyzedLibrary: String, libraryVersion: String)

  case class ElasticTypeData(fqn: String, isInstantiated: Boolean, parentType: Option[String], parentInterfaces: Set[String])

  case class InvocationObligation(declaredTypeName:String, methodDescription: String)

  case class ArtifactMetadata(libIdent: String, version: String, types: List[ElasticTypeData], dependencies: List[ArtifactDependency])

  case class ArtifactDependency(libIdent: String, version: String, scope: String)

}
