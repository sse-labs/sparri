package org.tud.cgcrawling

import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.callgraphs.{JreCallgraphBuilder, LibraryCallgraphBuilder}
import org.tud.cgcrawling.discovery.rabbitmq.MqMessageReader
import org.tud.cgcrawling.storage.{GraphDbStorageResult, HybridElasticAndGraphDbStorageHandler, StorageHandler}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class CallGraphCrawler(val configuration: Configuration){

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val storageHandler: StorageHandler = new HybridElasticAndGraphDbStorageHandler(configuration)

  def process(): Unit = {

    val reader = new MqMessageReader(configuration)
    var currentIdentifierTry = Try(reader.readNext())

    while(currentIdentifierTry.isSuccess && currentIdentifierTry.get.isDefined){

      val libraryIdentifier = currentIdentifierTry.get.get
      log.info(s"Got library identifier from queue: $libraryIdentifier")

      if(storageHandler.libraryExists(libraryIdentifier).getOrElse(false)){
        log.warn(s"Not processing $libraryIdentifier, as it is already in the database")
      } else {
        val parts = libraryIdentifier.split(":")
        val storageResult = processLibrary(parts(0),parts(1))

        if(!storageResult.success){
          log.error(s"Failed to store library callgraph ${storageResult.libraryName}")
          //TODO: Re-Publish / Save errors?
        }

      }

      currentIdentifierTry = Try(reader.readNext())
    }

    if(currentIdentifierTry.isFailure) {
      log.error("Unknown error while reading identifiers from queue.", currentIdentifierTry.failed.get)
    }
  }

  /*def startProcessing(): Future[Done]= {

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
            //TODO: Re-Publish / Save errors?
          }

        }
      }
  }*/

  def processLibrary(groupId: String, artifactId: String): GraphDbStorageResult = {

    // Instantiate the LibraryCallgraphBuilder only if the given library is not the JRE. If it is the JRE, we want to
    // use a dedicated builder
    var builder = if(!artifactId.equals("<jre>")) {
      Some(new LibraryCallgraphBuilder(groupId, artifactId, configuration))
    } else { None }

    // Build CG evolution, use the dedicated JreCallgraphBuilder if the current library is the JRE
    builder.map(_.buildCallgraphEvolution()).getOrElse(JreCallgraphBuilder.buildCallgraphEvolution()) match {

      case Success(evolution) =>

        // Shutdown builder (only necessary if not JRE)
        builder.foreach(_.shutdown())
        builder = null //Free unused class space as soon as possible

        // Print success and statistics
        log.info(s"Finished building CG evolution for ${evolution.libraryName}.")
        log.info(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.numberOfTypeEvolutions()} types, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")

        // Initiate storage
        storageHandler.storeCallGraphEvolution(evolution)

      case Failure(ex) =>
        log.error(s"Failed to read versions for library $groupId:$artifactId", ex)
        builder.foreach(_.shutdown())
        builder = null
        GraphDbStorageResult(s"$groupId:$artifactId", success = false)
    }

  }
}

object CallGraphCrawler {

  def main(args: Array[String]): Unit = {
    val theConfig = new Configuration()

    val crawler = new CallGraphCrawler(theConfig)
    crawler.process()
  }
}