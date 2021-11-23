package org.tud.cgcrawling

import akka.Done
import akka.actor.ActorSystem
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.callgraphs.LibraryCallgraphBuilder
import org.tud.cgcrawling.discovery.rabbitmq.MqIdentifierProcessing
import org.tud.cgcrawling.storage.{GraphDbStorageResult, HybridElasticAndGraphDbStorageHandler, StorageHandler}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success}

class CallGraphCrawler(val configuration: Configuration)
                      (implicit system: ActorSystem) extends  MqIdentifierProcessing {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val storageHandler: StorageHandler = new HybridElasticAndGraphDbStorageHandler(configuration)

  def startProcessing(): Future[Done]= {
    createSource(configuration)
      .runForeach { libraryIdentifier =>
        log.info(s"Got library identifier from queue: $libraryIdentifier")

        if(storageHandler.libraryExists(libraryIdentifier).getOrElse(false)){
          log.warn(s"Not processing $libraryIdentifier, as it is already in the database")
        } else {
          val parts = libraryIdentifier.split(":")
          val storageResult = processLibrary(parts(0),parts(1))

          if(!storageResult.success){
            log.error(s"Failed to store library callgraph ${storageResult.libraryName}")
          }

        }
      }
  }

  def processLibrary(groupId: String, artifactId: String): GraphDbStorageResult = {

    val libCgBuilder = new LibraryCallgraphBuilder(groupId, artifactId, configuration)

    libCgBuilder.buildCallgraphEvolution() match {

      case Success(evolution) =>

        libCgBuilder.shutdown()
        log.info(s"Finished building CG evolution for ${evolution.libraryName}.")
        log.info(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")
        storageHandler.storeCallGraphEvolution(evolution)

      case Failure(ex) =>
        log.error(s"Failed to read versions for library $groupId:$artifactId", ex)
        libCgBuilder.shutdown()
        GraphDbStorageResult(s"$groupId:$artifactId", success = false)
    }

  }
}

object CallGraphCrawler {

  implicit val theSystem: ActorSystem = ActorSystem("opal-cg-crawler")

  def main(args: Array[String]): Unit = {
    val theConfig = new Configuration()
    val shutdownTimeout = Timeout(1 minutes)

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