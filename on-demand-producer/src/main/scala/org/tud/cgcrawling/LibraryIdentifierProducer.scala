package org.tud.cgcrawling


import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.maven.DependencyListProcessor
import org.tud.cgcrawling.storage.MessageQueuePublisher

import java.io.File
import scala.language.postfixOps
import scala.util.{Failure, Success}

class LibraryIdentifierProducer {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Configuration = new Configuration()
  private val mqPublisher: MessageQueuePublisher = new MessageQueuePublisher(config)

  mqPublisher.initialize()

  def publishDependencies(pathToFile: String): Unit = {

    val depFile = new File(pathToFile)

    if(depFile.exists()){
      log.info(s"Publishing dependencies of dependency list file '$pathToFile' ...")
      val extractor = new DependencyListProcessor(depFile)

      extractor.getDependencies match {
        case Success(dependencyIdentifiers) =>
          dependencyIdentifiers
            .map(_.toLibIdent)
            .distinct
            .foreach(mqPublisher.publishIdentifier)

          log.info(s"Done publishing dependencies.")
        case Failure(ex) =>
          log.error("Failed to process dependencies.", ex)
      }
    } else {
      log.error(s"POM file does not exist at '$pathToFile'.")
    }


  }

  def shutdown(): Unit = mqPublisher.shutdown()

}

object LibraryIdentifierProducer {

  private val log = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {

    if(args.isEmpty){
      log.error("Usage: <dep-file-path> [<additional-dep-file-path*>]")
    } else {
      log.info(s"Processing ${args.length} dependency list files")
      val producer = new LibraryIdentifierProducer()

      for(file <- args){
        producer.publishDependencies(file)
      }

      producer.shutdown()
    }
  }
}
