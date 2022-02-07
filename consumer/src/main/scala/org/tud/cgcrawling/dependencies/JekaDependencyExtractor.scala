package org.tud.cgcrawling.dependencies
import dev.jeka.core.api.depmanagement.{JkDependencySet, JkQualifiedDependencySet, JkRepo}
import dev.jeka.core.api.depmanagement.resolution.JkDependencyResolver
import org.tud.cgcrawling.{Configuration, MavenRepositoryUriString}
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.model.DependencyIdentifier

import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.util.Try

object JekaDependencyExtractor extends DependencyExtractor {

  private def withResolver[T](implicit function: JkDependencyResolver[Void] => T): Try[T] = Try {
    val resolver = JkDependencyResolver.of().addRepos(JkRepo.ofMavenCentral())
    resolver.getParams.setFailOnDependencyResolutionError(false)

    function(resolver)
  }

  override def resolveDependencies(identifier: MavenIdentifier): Try[Dependencies] = withResolver { resolver =>
    resolver
      .resolve(JkDependencySet.of(identifier.toString)).getDependencyTree.getChildren().asScala.flatMap(p => p.getChildren.asScala).map( node => {
        val scope = node.getNodeInfo.getDeclaredConfigurations.asScala.head
        new DependencyIdentifier(new MavenIdentifier(MavenRepositoryUriString, node.getModuleInfo.getModuleId.getGroup,
          node.getModuleInfo.getModuleId.getName,node.getModuleInfo.getResolvedVersion.toString), scope)
      })
      .toList
      .filter(dep => !dep.identifier.equals(identifier))
  }

  def getDeclaredDependencies(identifier: MavenIdentifier): Try[Dependencies] = withResolver { resolver =>
     val dependencySet = JkQualifiedDependencySet.of(JkDependencySet.of(identifier.toString))
    //TODO: Test scope dependencies?
    resolver
      .resolve(dependencySet)
      .getDependencyTree.getChildren.asScala.flatMap(p => p.getChildren.asScala)
      .map( node => {
        val scope = node.getNodeInfo.getDeclaredConfigurations.asScala.head
        new DependencyIdentifier(new MavenIdentifier(MavenRepositoryUriString, node.getModuleInfo.getModuleId.getGroup,
          node.getModuleInfo.getModuleId.getName,node.getModuleInfo.getDeclaredVersion.toString), scope)
      })
      .toList
      .filter(dep => !dep.identifier.equals(identifier))
  }

  override def resolveAllDependencies(identifier: MavenIdentifier): (Try[Dependencies], Seq[MavenIdentifier]) = {

    (withResolver { resolver =>
      resolver
        .resolve(JkDependencySet.of(identifier.toString)).getDependencyTree.toFlattenList.asScala.map(node => {

        val scope = node.getNodeInfo.getDeclaredConfigurations.asScala.head
        new DependencyIdentifier(new MavenIdentifier(MavenRepositoryUriString, node.getModuleInfo.getModuleId.getGroup,
          node.getModuleInfo.getModuleId.getName, node.getModuleInfo.getResolvedVersion.toString), scope)
      })
        .toList
        .distinct
        .filter(dep => !dep.identifier.equals(identifier))
    }, Seq.empty)

  }

}