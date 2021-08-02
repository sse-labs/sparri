package org.tud.cgcrawling.dependencies

import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.resolution.ArtifactDescriptorRequest
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.model.DependencyIdentifier

import scala.collection.JavaConverters.{asScalaBufferConverter, seqAsJavaListConverter}
import scala.util.Try

class PomFileDependencyExtractor(configuration: Configuration) {

  def resolveDependencies(identifier: MavenIdentifier): Try[Seq[DependencyIdentifier]] = {

    val resolverRequest = new ArtifactDescriptorRequest()
    resolverRequest.setArtifact(new DefaultArtifact(identifier.toString))
    resolverRequest.setRepositories(List(AetherRepositoryProvider.centralRepository).asJava)

    Try{
      val result = AetherRepositoryProvider.repoSystem
        .readArtifactDescriptor(AetherRepositoryProvider.repoSession, resolverRequest)

      result
        .getDependencies
        .asScala
        .map { dependency =>
          val ident = new MavenIdentifier(configuration.mavenRepoBase.toString, dependency.getArtifact.getGroupId,
            dependency.getArtifact.getArtifactId, dependency.getArtifact.getVersion)

          new DependencyIdentifier(ident, dependency.getScope)
        }
    }
  }
}
