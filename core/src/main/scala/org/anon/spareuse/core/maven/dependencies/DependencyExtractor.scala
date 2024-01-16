package org.anon.spareuse.core.maven.dependencies

import org.anon.spareuse.core.maven.{MavenDependencyIdentifier, MavenIdentifier}
import org.anon.spareuse.core.maven.{MavenDependencyIdentifier, MavenIdentifier}

import scala.util.{Failure, Success, Try}

trait DependencyExtractor {
  type Dependencies = Seq[MavenDependencyIdentifier]
  type DependenciesAndFailures = (Try[Dependencies], Seq[MavenIdentifier])

  def resolveDependencies(identifier: MavenIdentifier): Try[Dependencies]

  def resolveAllDependencies(identifier: MavenIdentifier): DependenciesAndFailures = {
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
          .foldLeft((Success(Seq.empty[MavenDependencyIdentifier]), Seq.empty[MavenIdentifier]))( (a, b) => (Success((a._1.get ++ b._1).distinct), (a._2 ++ b._2).distinct))

      case fail@Failure(_) =>
        (fail, Seq.empty)
    }
  }
}
