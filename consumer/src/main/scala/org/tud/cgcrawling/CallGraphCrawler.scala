package org.tud.cgcrawling

import akka.Done
import akka.actor.ActorSystem
import akka.util.Timeout
import org.opalj.log.{GlobalLogContext, OPALLogger}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.discovery.maven.LibraryArtifactProcessing
import org.tud.cgcrawling.discovery.rabbitmq.MqIdentifierProcessing
import org.tud.cgcrawling.download.MavenJarDownloader
import org.tud.cgcrawling.graphgeneration.{CallGraphBuilder, OPALLogAdapter}
import org.tud.cgcrawling.model.LibraryCallGraphEvolution
import org.tud.cgcrawling.storage.{GraphDbStorageHandler, GraphDbStorageResult}

import java.net.URI
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success}

class CallGraphCrawler(val configuration: Configuration)
                      (implicit system: ActorSystem)
  extends LibraryArtifactProcessing with MqIdentifierProcessing {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val processingTimeout: Timeout = Timeout(24 hours)

  def startProcessing(): Future[Done]= {
    createSource(configuration)
      .map { libraryIdentifier =>
        log.info(s"Got library identifier from queue: $libraryIdentifier")
        /*val parts = libraryIdentifier.split(":")
        val storageResult = Await.result(processLibrary(parts(0),parts(1)), processingTimeout.duration)

        if(!storageResult.success){
          log.error(s"Failed to store library callgraph ${storageResult.libraryName}")
        }*/
      }
      .run()
  }

  def processLibrary(groupId: String, artifactId: String): Future[GraphDbStorageResult] = {
    val theCallGraphEvolution = new LibraryCallGraphEvolution(groupId, artifactId)
    val downloader = new MavenJarDownloader()
    val cgBuilder = new CallGraphBuilder(configuration)
    val cgStorageHandler = new GraphDbStorageHandler(configuration)
    createIdentifierSource(groupId, artifactId) match {

      case Success(identifierSource) =>
        val processing = identifierSource
          .map(downloader.downloadJarFile)
          .filter(_.jarFile.isDefined)
          .map(cgBuilder.buildCallgraph)
          .filter(_.success)
          .runForeach{ result =>
            theCallGraphEvolution.applyNewRelease(result.callgraph.get,
              result.project.get, result.identifier.version)
          }

        processing
          .map { _ =>
            downloader.shutdown()
            log.info(s"Finished building CG evolution for ${theCallGraphEvolution.libraryName}.")
            log.info(s"Got a total of ${theCallGraphEvolution.releases().size} releases with ${theCallGraphEvolution.numberOfMethodEvolutions()} methods and ${theCallGraphEvolution.numberOfInvocationEvolutions()} invocations")
            cgStorageHandler.storeCallGraphEvolution(theCallGraphEvolution)
          }(system.dispatcher)


      case Failure(ex) =>
        log.error(s"Failed to read versions for library $groupId:$artifactId", ex)
        downloader.shutdown()
        Future.failed(ex)
    }

  }

  override val repoUri: URI = configuration.mavenRepoBase
}

object CallGraphCrawler {

  implicit val theSystem: ActorSystem = ActorSystem("opal-cg-crawler")

  def main(args: Array[String]) = {
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