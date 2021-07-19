package org.tud.cgcrawling

import akka.actor.{ActorRef, ActorSystem}
import akka.event.LoggingAdapter
import akka.pattern.ask
import akka.routing.SmallestMailboxPool
import akka.stream.scaladsl.Sink
import akka.stream.{ActorMaterializer, Materializer}
import akka.util.Timeout
import org.tud.cgcrawling.discovery.maven.{IndexProcessing, MavenIdentifier}
import org.tud.cgcrawling.download.{MavenDownloadActor, MavenDownloadActorResponse}
import org.tud.cgcrawling.graphgeneration.{CallGraphActor, CallGraphActorResponse, OPALLogAdapter}
import org.tud.cgcrawling.storage.{CallGraphStorageActor, CallGraphStorageActorResponse, CallGraphStorageQueries}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class CallGraphCrawler(val configuration: Configuration)
                      (implicit system: ActorSystem)
  extends IndexProcessing with AppLogging with CallGraphStorageQueries {

  private val artifactsSeen  = mutable.HashSet[MavenIdentifier]()

  val downloaderPool: ActorRef =
    system.actorOf(SmallestMailboxPool(configuration.numberOfDownloadThreads).props(MavenDownloadActor.props))

  val cgGeneratorPool: ActorRef =
    system.actorOf(SmallestMailboxPool(configuration.numberOfCgThreads).props(CallGraphActor.props(configuration)))

  val storagePool: ActorRef =
    system.actorOf(SmallestMailboxPool(configuration.numberOfStorageThreads).props(CallGraphStorageActor.props(configuration)))

  def doCrawling() = {

    implicit val materializer: Materializer = ActorMaterializer()
    implicit val logger: LoggingAdapter = log
    implicit val timeout: Timeout = Timeout(30 minutes)

    val identifierSource =
      createSource(configuration.mavenRepoBase)
        .filter(identifier => !artifactGAVExistsInDb(identifier.toString))
        .filter(identifier => {
          val hasBeenSeen = artifactsSeen.contains(identifier)
          if(!hasBeenSeen) artifactsSeen.add(identifier)
          !hasBeenSeen
        })
        .throttle(configuration.throttle.element, configuration.throttle.per)

    identifierSource
      .mapAsyncUnordered(configuration.numberOfDownloadThreads)(identifier => (downloaderPool ? identifier)
        .mapTo[MavenDownloadActorResponse])
      .filter(response => !response.jarDownloadFailed && response.artifact.isDefined)
      .mapAsyncUnordered(configuration.numberOfCgThreads)(downloadResponse => (cgGeneratorPool ? downloadResponse.artifact.get)
        .mapTo[CallGraphActorResponse])
      .filter(response => response.success)
      .mapAsyncUnordered(configuration.numberOfStorageThreads)(cgResponse => (storagePool ? cgResponse)
        .mapTo[CallGraphStorageActorResponse])
      .to(Sink.ignore)
      .run()
    log.info("Done")
  }

}

object CallGraphCrawler {
  val theSystem: ActorSystem = ActorSystem("opal-cg-crawler")

  def main(args: Array[String]) = {
    val theConfig = new Configuration()

    val theApp = new CallGraphCrawler(theConfig)(theSystem)

    theApp.doCrawling()
  }
}