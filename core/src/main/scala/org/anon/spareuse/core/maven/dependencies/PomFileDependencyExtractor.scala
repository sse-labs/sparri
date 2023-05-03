package org.anon.spareuse.core.maven.dependencies

import com.squareup.tools.maven.resolution.ArtifactResolver
import org.anon.spareuse.core.maven.MavenIdentifier.DefaultRepository
import org.anon.spareuse.core.maven.{MavenDependencyIdentifier, MavenIdentifier}
import org.anon.spareuse.core.maven.MavenIdentifier

import scala.jdk.CollectionConverters.asScalaBufferConverter
import scala.util.Try

class PomFileDependencyExtractor extends DependencyExtractor {

  private val theResolver = new ArtifactResolver()

  override def resolveDependencies(identifier: MavenIdentifier): Try[Dependencies] = Try {

    val theArtifact = theResolver.artifactFor(identifier.toString)
    val resolvedArtifact = theResolver.resolve(theArtifact)

    resolvedArtifact.component2().getModel.getDependencies.asScala.map { dependency =>
      val ident = new MavenIdentifier(DefaultRepository, dependency.getGroupId, dependency.getArtifactId, dependency.getVersion)

      MavenDependencyIdentifier(ident, dependency.getScope)
    }
  }

}
