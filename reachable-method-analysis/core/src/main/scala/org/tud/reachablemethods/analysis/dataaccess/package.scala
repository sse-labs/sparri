package org.tud.reachablemethods.analysis

package object dataaccess {

  case class ElasticMethodData(elasticId: String, name: String, signature: String, isExtern: Boolean, obligations: Iterable[InvocationObligation],
                               calleeSignatures: Iterable[String], definingLibrary: String, analyzedLibrary: String, libraryVersion: String)

  case class InvocationObligation(declaredTypeName:String, methodDescription: String)

  case class ArtifactMetadata(libIdent: String, version: String, instantiatedTypes: List[String], dependencies: List[ArtifactDependency])

  case class ArtifactDependency(libIdent: String, version: String, scope: String)

}
