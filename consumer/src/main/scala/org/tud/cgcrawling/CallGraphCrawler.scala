package org.tud.cgcrawling

import akka.Done
import akka.actor.ActorSystem
import akka.util.Timeout
import org.opalj.log.{GlobalLogContext, OPALLogger}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.dependencies.PomFileDependencyExtractor
import org.tud.cgcrawling.discovery.maven.LibraryArtifactProcessing
import org.tud.cgcrawling.discovery.rabbitmq.MqIdentifierProcessing
import org.tud.cgcrawling.download.MavenJarDownloader
import org.tud.cgcrawling.graphgeneration.{CallGraphBuilder, OPALLogAdapter}
import org.tud.cgcrawling.model.{DependencyIdentifier, LibraryCallGraphEvolution}
import org.tud.cgcrawling.storage.{GraphDbStorageHandler, GraphDbStorageResult, HybridElasticAndGraphDbStorageHandler, StorageHandler}

import java.net.URI
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success}

class CallGraphCrawler(val configuration: Configuration)
                      (implicit system: ActorSystem)
  extends LibraryArtifactProcessing with MqIdentifierProcessing {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val storageHandler: StorageHandler = new HybridElasticAndGraphDbStorageHandler(configuration)

  def startProcessing(): Future[Done]= {
    createSource(configuration)
      .runForeach { libraryIdentifier =>
        log.info(s"Got library identifier from queue: $libraryIdentifier")
        val parts = libraryIdentifier.split(":")
        val storageResult = processLibrary(parts(0),parts(1))

        if(!storageResult.success){
          log.error(s"Failed to store library callgraph ${storageResult.libraryName}")
        }
      }
  }

  def processLibrary(groupId: String, artifactId: String): GraphDbStorageResult = {
    val theCallGraphEvolution = new LibraryCallGraphEvolution(groupId, artifactId)


    val downloader = new MavenJarDownloader()
    val dependencyExtractor = new PomFileDependencyExtractor(configuration)
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
            val cgResponse = new CallGraphBuilder(configuration, system).buildCallgraph(downloadResponse)

            if(cgResponse.success) {
              theCallGraphEvolution.applyNewRelease(cgResponse.callgraph.get, cgResponse.project.get,
                dependencies, identifier.version)
            }
          }
        }

        downloader.shutdown()
        log.info(s"Finished building CG evolution for ${theCallGraphEvolution.libraryName}.")
        log.info(s"Got a total of ${theCallGraphEvolution.numberOfDependencyEvolutions()} dependencies, ${theCallGraphEvolution.releases().size} releases with ${theCallGraphEvolution.numberOfMethodEvolutions()} methods and ${theCallGraphEvolution.numberOfInvocationEvolutions()} invocations")
        storageHandler.storeCallGraphEvolution(theCallGraphEvolution)

      case Failure(ex) =>
        log.error(s"Failed to read versions for library $groupId:$artifactId", ex)
        downloader.shutdown()
        GraphDbStorageResult(theCallGraphEvolution.libraryName, success = false)
    }

  }

  override val repoUri: URI = configuration.mavenRepoBase
}

object CallGraphCrawler {

  implicit val theSystem: ActorSystem = ActorSystem("opal-cg-crawler")

  def main(args: Array[String]): Unit = {
    val theConfig = new Configuration()
    val shutdownTimeout = Timeout(1 minutes)

    OPALLogger.updateLogger(GlobalLogContext, OPALLogAdapter)

    val crawler = new CallGraphCrawler(theConfig)
    crawler
      .startProcessing()
      .onComplete{ response =>
        println(s"Got success value ${response.isSuccess}")
        theConfig.graphDatabaseDriver.close()
        Await.result(theSystem.terminate(), shutdownTimeout.duration)
      }(theSystem.dispatcher)


  }
}