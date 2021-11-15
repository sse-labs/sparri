package org.tud.cgcrawling.dependencies

import com.squareup.tools.maven.resolution.ArtifactResolver
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.model.DependencyIdentifier

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.{Failure, Success, Try}

class PomFileDependencyExtractor(configuration: Configuration) extends DependencyExtractor {

  private val theResolver = new ArtifactResolver()

  override def resolveDependencies(identifier: MavenIdentifier): Try[Dependencies] = Try {

    val theArtifact = theResolver.artifactFor(identifier.toString)
    val resolvedArtifact = theResolver.resolve(theArtifact)

    resolvedArtifact.component2().getModel.getDependencies.asScala.map { dependency =>
      val ident = new MavenIdentifier(configuration.mavenRepoBase.toString, dependency.getGroupId,
        dependency.getArtifactId, dependency.getVersion)

      new DependencyIdentifier(ident, dependency.getScope)
    }
  }

}
