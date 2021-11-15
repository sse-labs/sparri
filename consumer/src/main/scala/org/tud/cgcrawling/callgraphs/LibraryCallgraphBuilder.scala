package org.tud.cgcrawling.callgraphs

import akka.actor.ActorSystem
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.dependencies.JekaDependencyExtractor
import org.tud.cgcrawling.discovery.maven.{LibraryArtifactProcessing, MavenIdentifier}
import org.tud.cgcrawling.download.MavenJarDownloader
import org.tud.cgcrawling.model.{DependencyIdentifier, LibraryCallGraphEvolution}
import org.tud.cgcrawling.opal.OPALProjectHelper
import org.tud.cgcrawling.opal.OPALProjectHelper.ClassList

import java.net.URI
import scala.util.{Failure, Success, Try}

class LibraryCallgraphBuilder(groupId: String,
                              artifactId: String,
                              config: Configuration)(implicit system: ActorSystem) extends LibraryArtifactProcessing {

  override val repoUri: URI = config.mavenRepoBase

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val classFileCache: ArtifactClassfileCache = new ArtifactClassfileCache(20)
  private val downloader: MavenJarDownloader = new MavenJarDownloader()
  private val dependencyExtractor: JekaDependencyExtractor = new JekaDependencyExtractor(config)

  def buildCallgraphEvolution(): Try[LibraryCallGraphEvolution] = {
    val theCallGraphEvolution = new LibraryCallGraphEvolution(groupId, artifactId)

    createIdentifierIterator(groupId, artifactId) match {

      case Success(identifierIterable) =>
        identifierIterable.foreach(i => processIdentifier(i, theCallGraphEvolution))

        log.info(s"Got cache hit rate of ${classFileCache.hitRate()} for library $groupId:$artifactId")

        Success(theCallGraphEvolution)

      case Failure(ex) =>
        log.error(s"Failed to read versions for library $groupId:$artifactId")
        Failure(ex)
    }
  }

  private[callgraphs] def processIdentifier(identifier: MavenIdentifier,
                        evolution: LibraryCallGraphEvolution): Unit = {
    val downloadResponse = downloader.downloadJar(identifier)

    // Get dependencies
    val dependencies = dependencyExtractor.getDeclaredDependencies(identifier) match {
      case Success(dependencies) => dependencies.toSet
      case Failure(ex) =>
        log.error(s"Failed to extract dependencies for release ${identifier.version} of library ${evolution.libraryName}", ex)
        Set.empty[DependencyIdentifier]
    }

    // We only need to go into analysis if this artifact is a JAR file. If not, we can skip it and just record the dependencies
    if(downloadResponse.jarFile.isDefined){
      // Get all third party classes to do whole-program analysis
      val allThirdPartyClasses = getAllThirdPartyClassesWithCache(identifier)

      // Build Callgraph for entire program
      val cgResponse = new CallGraphBuilder(config, system).buildCallgraph(downloadResponse, allThirdPartyClasses)

      // Apply the callgraph to the library  evolution object if successful
      if(cgResponse.success) {
        evolution.applyNewRelease(cgResponse.callgraph.get, cgResponse.project.get,
          dependencies, identifier.version)
      }
    }
  }

  private[callgraphs] def getAllThirdPartyClassesWithCache(identifier: MavenIdentifier): ClassList = {
    dependencyExtractor.resolveAllDependencies(identifier)._1 match {
      case Success(allDependencies) =>
        allDependencies
          .map(_.identifier)
          .flatMap( ident => {

            classFileCache.getEntry(ident).getOrElse{
              val response = downloader.downloadJar(ident)
              if(response.jarFile.isDefined){
                val classes = OPALProjectHelper.readClassesFromJarStream(response.jarFile.get.is, ident.toJarLocation.toURL) match {
                  case Success(cfs) => cfs
                  case Failure(ex) =>
                    log.error("Failed to read class files from JAR " + ident.toString + ": " + ex.getMessage)
                    List.empty
                }
                classFileCache.pushEntry(ident, classes)
              } else {
                log.warn("Non JAR dependency: " + ident.toString)
                List.empty
              }
            }

          })
          .toList
      case Failure(ex) =>
        log.error("Failed to calculate all dependencies for " + identifier.toString + ": " + ex.getMessage)
        List.empty
    }
  }

  def shutdown(): Unit = downloader.shutdown()
}
