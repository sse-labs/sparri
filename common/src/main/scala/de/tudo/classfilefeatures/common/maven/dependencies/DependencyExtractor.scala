package de.tudo.classfilefeatures.common.maven.dependencies

import de.tudo.classfilefeatures.common.maven.model.{MavenDependencyIdentifier, MavenIdentifier}

import scala.util.{Failure, Success, Try}

trait DependencyExtractor {
  type Dependencies = Seq[MavenDependencyIdentifier]

  def resolveDependencies(identifier: MavenIdentifier): Try[Dependencies]

  def resolveAllDependencies(identifier: MavenIdentifier): (Try[Dependencies], Seq[MavenIdentifier]) = {
    resolveDependencies(identifier) match {
      case Success(dependencies) =>
        dependencies
          .map( dependency => {
            if(!dependency.scope.equals("test")){
              resolveAllDependencies(dependency.identifier) match {
                case result if result._1.isSuccess =>
                  (Seq(dependency) ++ result._1.get, result._2)
                case result =>
                  (Seq(), Seq(dependency.identifier) ++ result._2)
              }
            } else {
              (Seq(), Seq())
            }
          })
          .aggregate((Success(Seq.empty[MavenDependencyIdentifier]), Seq.empty[MavenIdentifier]))((a, b) => (Success((a._1.get ++ b._1).distinct), (a._2 ++ b._2).distinct), (a,b) => (Success((a._1.get ++ b._1.get).distinct), (a._2 ++ b._2).distinct))

      case fail@Failure(_) =>
        (fail, Seq.empty)
    }
  }
}
