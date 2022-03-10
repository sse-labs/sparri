package org.tud.cgcrawling

import org.slf4j.Logger
import org.tud.cgcrawling.callgraphs.CallGraphBuilder
import org.tud.cgcrawling.dependencies.JekaDependencyExtractor
import org.tud.cgcrawling.discovery.maven.LibraryArtifactProcessing
import org.tud.cgcrawling.download.MavenJarDownloader
import org.tud.cgcrawling.model.{DependencyIdentifier, LibraryCallGraphEvolution}
import org.tud.cgcrawling.opal.OPALProjectHelper

import java.net.URI
import scala.util.{Failure, Success}

package object storage extends LibraryArtifactProcessing{

  def buildEvolutionFor(groupId: String, artifactId: String, configuration: Configuration)
                       (log: Logger): Option[LibraryCallGraphEvolution] = {

    val theCallGraphEvolution = new LibraryCallGraphEvolution(groupId, artifactId)
    val opalHelper = new OPALProjectHelper


    val downloader = new MavenJarDownloader()
    createIdentifierIterator(groupId, artifactId) match {

      case Success(identifierIterable) =>
        for(identifier <- identifierIterable){
          val downloadResponse = downloader.downloadJar(identifier)

          val dependencies = new JekaDependencyExtractor {}.resolveDependencies(identifier) match {
            case Success(dependencies) =>
              dependencies.toSet
            case Failure(ex) =>
              log.error(s"Failed to extract dependencies for release ${identifier.version} of library ${theCallGraphEvolution.libraryName}", ex)
              Set.empty[DependencyIdentifier]
          }

          if(downloadResponse.jarFile.isDefined){
            val cgResponse = CallGraphBuilder.buildCallgraph(downloadResponse, List.empty, Map.empty, opalHelper)

            if(cgResponse.success) {
              theCallGraphEvolution.applyNewRelease(cgResponse.callgraph.get, dependencies, identifier.version)
            }
          }
        }

        downloader.shutdown()
        Some(theCallGraphEvolution)

      case Failure(ex) =>
        log.error("Failed to read artifacts for library", ex)
        None
    }
  }

  override val repoUri: URI = new URI("https://repo1.maven.org/maven2/")
}
