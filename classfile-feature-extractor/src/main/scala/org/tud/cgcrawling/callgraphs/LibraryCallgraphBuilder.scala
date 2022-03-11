package org.tud.cgcrawling.callgraphs

import de.tudo.classfilefeatures.common.download.MavenJarDownloader
import de.tudo.classfilefeatures.common.maven.dependencies.JekaDependencyExtractor
import de.tudo.classfilefeatures.common.maven.model.{MavenDependencyIdentifier, MavenIdentifier}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.LibraryArtifactProcessing
import org.tud.cgcrawling.model.LibraryCallGraphEvolution
import de.tudo.classfilefeatures.common.opal.OPALProjectHelper
import de.tudo.classfilefeatures.common.opal.OPALProjectHelper.ClassList

import java.net.URI
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class LibraryCallgraphBuilder(groupId: String,
                              artifactId: String,
                              config: Configuration) extends LibraryArtifactProcessing with JekaDependencyExtractor {

  override val repoUri: URI = config.mavenRepoBase

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private[callgraphs] val classFileCache: ArtifactClassfileCache = new ArtifactClassfileCache(maxCacheSize = 12)
  private[callgraphs] val downloader: MavenJarDownloader = new MavenJarDownloader()
  private[callgraphs] val opalHelper: OPALProjectHelper = new OPALProjectHelper()

  private val classFqnToDependencyMap: mutable.Map[String, MavenIdentifier] = new mutable.HashMap()

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
    val dependencies = getDeclaredDependencies(identifier) match {
      case Success(dependencies) => dependencies.toSet
      case Failure(ex) =>
        log.error(s"Failed to extract dependencies for release ${identifier.version} of library ${evolution.libraryName}", ex)
        Set.empty[MavenDependencyIdentifier]
    }

    // We only need to go into analysis if this artifact is a JAR file. If not, we can skip it and just record the dependencies
    if(downloadResponse.jarFile.isDefined){
      // Clear class FQN to dependency translation map
      classFqnToDependencyMap.clear()
      // Get all third party classes to do whole-program analysis
      val allThirdPartyClasses = getAllThirdPartyClassesWithCache(identifier)

      // Build Callgraph for entire program
      val cgResponse = CallGraphBuilder.buildCallgraph(downloadResponse, allThirdPartyClasses, classFqnToDependencyMap.toMap, opalHelper)

      // Apply the callgraph to the library  evolution object if successful
      if(cgResponse.success) {
        evolution.applyNewRelease(cgResponse.callgraph.get, dependencies, identifier.version)
      }
    }
  }

  private[callgraphs] def getAllThirdPartyClassesWithCache(identifier: MavenIdentifier, loadImplementation: Boolean = false): ClassList = {
    resolveAllDependencies(identifier)._1 match {
      case Success(allDependencies) =>
        allDependencies
          .map(_.identifier)
          .flatMap( ident => {

            val classes = classFileCache.getEntry(ident).getOrElse{
              val response = downloader.downloadJar(ident)
              if(response.jarFile.isDefined){
                val classes = opalHelper.readClassesFromJarStream(response.jarFile.get.is, ident.toJarLocation.toURL, loadImplementation) match {
                  case Success(cfs) =>
                    cfs
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

            classes.map(_._1.fqn).foreach(fqn => classFqnToDependencyMap.put(fqn, ident))

            classes
          })
          .toList
      case Failure(ex) =>
        log.error("Failed to calculate all dependencies for " + identifier.toString + ": " + ex.getMessage)
        List.empty
    }
  }

  def shutdown(): Unit = {
    classFileCache.clear()
    classFqnToDependencyMap.clear()
    downloader.shutdown()
    opalHelper.freeOpalResources()
  }
}
