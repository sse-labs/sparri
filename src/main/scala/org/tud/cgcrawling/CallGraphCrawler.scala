package org.tud.cgcrawling

import akka.actor.{ActorRef, ActorSystem}
import akka.event.LoggingAdapter
import akka.pattern.ask
import akka.routing.{RoundRobinPool, SmallestMailboxPool}
import akka.stream.scaladsl.Sink
import akka.stream.{ActorMaterializer, Materializer, OverflowStrategy}
import akka.util.Timeout
import org.opalj.log.{GlobalLogContext, OPALLogger}
import org.tud.cgcrawling.discovery.maven.{IndexProcessing, MavenIdentifier}
import org.tud.cgcrawling.download.{MavenDownloadActor, MavenDownloadActorResponse}
import org.tud.cgcrawling.graphgeneration.{CallGraphActor, CallGraphActorResponse, OPALLogAdapter}
import org.tud.cgcrawling.storage.{CallGraphStorageActor, CallGraphStorageActorResponse, CallGraphStorageQueries, ErrorStorageActor}
import org.tud.cgcrawling.utils.StreamingSignals.{Ack, StreamFailure}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class CallGraphCrawler(val configuration: Configuration)
                      (implicit system: ActorSystem)
  extends IndexProcessing with AppLogging with CallGraphStorageQueries {

  private val artifactsSeen  = mutable.HashSet[MavenIdentifier]()

  private val storageRouter = SmallestMailboxPool(configuration.numberOfStorageThreads)

  val downloaderPool: ActorRef =
    //system.actorOf(SmallestMailboxPool(configuration.numberOfDownloadThreads).props(MavenDownloadActor.props))
    system.actorOf(MavenDownloadActor.props.withRouter(SmallestMailboxPool(configuration.numberOfDownloadThreads)), name = "download-pool")

  val errorStoragePool: ActorRef =
    //system.actorOf(SmallestMailboxPool(configuration.numberOfStorageThreads).props(ErrorStorageActor.props(configuration)))
    system.actorOf(ErrorStorageActor.props(configuration).withRouter(SmallestMailboxPool(configuration.numberOfStorageThreads*2)), name = "error-storage-pool")

  val cgGeneratorPool: ActorRef =
    //system.actorOf(SmallestMailboxPool(configuration.numberOfCgThreads).props(CallGraphActor.props(configuration)))
    system.actorOf(CallGraphActor.props(configuration)
      .withRouter(SmallestMailboxPool(configuration.numberOfCgThreads)), name = "cg-generator-pool")

  val storagePool: ActorRef = {
    //system.actorOf(SmallestMailboxPool(configuration.numberOfStorageThreads).props(CallGraphStorageActor.props(configuration)))
    system.actorOf(CallGraphStorageActor.props(configuration).withRouter(storageRouter).withDispatcher("storage-dispatcher"), name = "cg-storage-pool")
  }

  def doCrawling() = {

    //implicit val materializer: Materializer = ActorMaterializer()
    implicit val logger: LoggingAdapter = log
    implicit val timeout: Timeout = Timeout(120 minutes)

    val identifierSource = {
      // Create source from Lucene index reader
      createSource(configuration.mavenRepoBase)
        // Filter for identifiers not yet present in DB
        .filter(identifier => {
          val keep = !artifactGAVExistsInDb(identifier.toString)

          if(!keep){
            log.info(s"Not processing ${identifier.toString}, already in DB")
          }

          keep
        })
        // Compensate for lag
        .filter(identifier => {
          val hasBeenSeen = artifactsSeen.contains(identifier)
          if(!hasBeenSeen) artifactsSeen.add(identifier)
          !hasBeenSeen
        })
        // Throttle production of new identifiers
        .throttle(configuration.throttle.element, configuration.throttle.per)
    }

    identifierSource
      // Download POM and JAR for all identifiers
      .mapAsyncUnordered(configuration.numberOfDownloadThreads)(identifier => (downloaderPool ? identifier)
        .mapTo[MavenDownloadActorResponse])
      // Pipe through error storage actor, which will store any download-related errors
      .mapAsyncUnordered(configuration.numberOfStorageThreads)(response => (errorStoragePool ? response)
        .mapTo[MavenDownloadActorResponse])
      // Filter for successful downloads
      .filter(response => !response.jarDownloadFailed && response.artifact.isDefined && response.artifact.get.jarFile.isDefined)
      // Generate Callgraphs for all JAR files
      .mapAsyncUnordered(configuration.numberOfCgThreads)(downloadResponse => (cgGeneratorPool ? downloadResponse.artifact.get)
        .mapTo[CallGraphActorResponse])
      // Pipe through error storage actor, which will store any callgraph-related errors
      .mapAsyncUnordered(configuration.numberOfStorageThreads)(response => (errorStoragePool ? response)
        .mapTo[CallGraphActorResponse])
      // Filter for successful cg generations
      .filter(response => response.success)
      // Pipe through storage actor to store callgraphs
      .mapAsyncUnordered(configuration.numberOfStorageThreads)(cgResponse => (storagePool ? cgResponse)
        .mapTo[CallGraphStorageActorResponse])
      // Dispose elements at this point
      .runWith(Sink.ignore)

    log.info("Done")
  }

}

object CallGraphCrawler {
  val theSystem: ActorSystem = ActorSystem("opal-cg-crawler")

  def main(args: Array[String]) = {
    val theConfig = new Configuration()

    OPALLogger.updateLogger(GlobalLogContext, OPALLogAdapter)

    val theApp = new CallGraphCrawler(theConfig)(theSystem)

    theApp.doCrawling()
  }
}