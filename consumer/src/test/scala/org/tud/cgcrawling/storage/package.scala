package org.tud.cgcrawling

import akka.actor.ActorSystem
import org.slf4j.Logger
import org.tud.cgcrawling.callgraphs.CallGraphBuilder
import org.tud.cgcrawling.dependencies.{JekaDependencyExtractor, PomFileDependencyExtractor}
import org.tud.cgcrawling.discovery.maven.LibraryArtifactProcessing
import org.tud.cgcrawling.download.MavenJarDownloader
import org.tud.cgcrawling.model.{DependencyIdentifier, LibraryCallGraphEvolution}

import java.net.URI
import scala.util.{Failure, Success}

package object storage extends LibraryArtifactProcessing{

  def buildEvolutionFor(groupId: String, artifactId: String, configuration: Configuration)
                       (implicit system: ActorSystem, log: Logger): Option[LibraryCallGraphEvolution] = {

    val theCallGraphEvolution = new LibraryCallGraphEvolution(groupId, artifactId)


    val downloader = new MavenJarDownloader()
    val dependencyExtractor = new JekaDependencyExtractor(configuration)
    createIdentifierIterator(groupId, artifactId) match {

      case Success(identifierIterable) =>
        for(identifier <- identifierIterable){
          val downloadResponse = downloader.downloadJar(identifier)

          val dependencies = dependencyExtractor.resolveDependencies(identifier) match {
            case Success(dependencies) =>
              dependencies.toSet
            case Failure(ex) =>
              log.error(s"Failed to extract dependencies for release ${identifier.version} of library ${theCallGraphEvolution.libraryName}", ex)
              Set.empty[DependencyIdentifier]
          }

          if(downloadResponse.jarFile.isDefined){
            val cgResponse = new CallGraphBuilder(configuration, system).buildCallgraph(downloadResponse, List.empty, Map.empty)

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
