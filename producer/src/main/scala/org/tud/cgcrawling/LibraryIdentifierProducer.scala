package org.tud.cgcrawling

import akka.Done
import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.discovery.maven.IndexProcessing
import org.tud.cgcrawling.storage.MessageQueuePublisher

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

class LibraryIdentifierProducer (configuration: Configuration) extends IndexProcessing {

  private val identifiersSeen: mutable.Set[String] = new mutable.HashSet[String]
  private var libCount: Int = 0
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  val publisher: MessageQueuePublisher = new MessageQueuePublisher(configuration)

  publisher.initialize()

  def publishAllIdentifiers(implicit system: ActorSystem): Future[Done] = {
    createSource(configuration.mavenRepoBase)
      .map(ident => s"${ident.groupId}:${ident.artifactId}")
      .filter{ ident =>
        if(identifiersSeen.contains(ident)){
          false
        } else {
          identifiersSeen.add(ident)
          true
        }
      }
      .map{ ident =>
        libCount += 1

        if(libCount % 100 == 0)
          log.info(s"Processing library identifier $libCount")

        ident
      }
      .runForeach(publisher.publishLibraryIdentifier)
  }



  def shutdown(): Unit = {
    publisher.shutdown()
  }

}

object LibraryIdentifierProducer {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("incremental-cg-creator-producer")
    val config = new Configuration()

    val producer = new LibraryIdentifierProducer(config)

    Await.ready(producer.publishAllIdentifiers(system), Duration.Inf)

    producer.shutdown()

    Await.ready(system.terminate(), 30 seconds)
  }
}
