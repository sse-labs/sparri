package de.tudo.classfilefeatures.common.maven.dependencies

import com.squareup.tools.maven.resolution.ArtifactResolver
import de.tudo.classfilefeatures.common.maven.model.{MavenDependencyIdentifier, MavenIdentifier}

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.Try

class PomFileDependencyExtractor extends DependencyExtractor {

  private val theResolver = new ArtifactResolver()

  override def resolveDependencies(identifier: MavenIdentifier): Try[Dependencies] = Try {

    val theArtifact = theResolver.artifactFor(identifier.toString)
    val resolvedArtifact = theResolver.resolve(theArtifact)

    resolvedArtifact.component2().getModel.getDependencies.asScala.map { dependency =>
      val ident = new MavenIdentifier(dependency.getGroupId, dependency.getArtifactId, dependency.getVersion)

      MavenDependencyIdentifier(ident, dependency.getScope)
    }
  }

}
