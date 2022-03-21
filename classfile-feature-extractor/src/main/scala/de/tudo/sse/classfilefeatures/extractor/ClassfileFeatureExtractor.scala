package de.tudo.sse.classfilefeatures.extractor

import akka.Done
import akka.actor.ActorSystem
import akka.stream.ActorAttributes.supervisionStrategy
import akka.stream.{Materializer, Supervision}
import akka.stream.scaladsl.Sink
import de.tudo.sse.classfilefeatures.common.download.{MavenDownloadResult, MavenJarDownloader}
import de.tudo.sse.classfilefeatures.common.maven.model.MavenIdentifier
import de.tudo.sse.classfilefeatures.common.model.ToModelConversion
import de.tudo.sse.classfilefeatures.common.opal.OPALProjectHelper
import de.tudo.sse.classfilefeatures.common.rabbitmq.MqStreamingSupport
import de.tudo.sse.classfilefeatures.extractor.model.LibraryClassfileFeatureModel
import de.tudo.sse.classfilefeatures.extractor.storage.ClassfileFeatureStorageHandler
import de.tudo.sse.classfilefeatures.extractor.storage.impl.PostgreSqlStorageHandler
import de.tudo.sse.classfilefeatures.extractor.utils.{IdempotentComputationResult, MavenArtifactDiscovery}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class ClassfileFeatureExtractor extends MqStreamingSupport with MavenArtifactDiscovery with ToModelConversion {

  private final val configuration: Configuration = new Configuration()
  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private[extractor] final val storageHandler: ClassfileFeatureStorageHandler = new PostgreSqlStorageHandler(configuration)
  private[extractor] final val mavenDownloader: MavenJarDownloader = new MavenJarDownloader()
  private[extractor] final val opalProjectHelper: OPALProjectHelper = new OPALProjectHelper(loadJreClassImplementation = false)

  private[extractor] final val actorSystem: ActorSystem = ActorSystem("classfile-feature-extractor")
  private[extractor] final val streamMaterializer: Materializer = Materializer(actorSystem)

  /**
   * Initializes all resources needed for this component. Will throw an exception if fatal errors are encountered.
   */
  def initialize(): Unit = {
    storageHandler.verifyConnectivity()
    storageHandler.initialize()
    log.info("Successfully initialized extractor.")
  }

  /**
   * Starts the processing phase of this component. The extractor will start receiving identifiers from the MessageQueue,
   * and will process them by downloading all releases for each library. The resulting Future is returned.
   * @return Future that completes once the message queue is empty or a fatal error is encountered
   */
  def startProcessingLibraries(): Future[Done] = {
    val libraryIdentifierSource = createMqMessageSource(configuration)

    val storageSink = buildStorageSink()

    libraryIdentifierSource
      .mapAsyncUnordered(configuration.numberOfTransformerThreads)(buildLibraryFeatureModel)
      .filter(result => result.successfullyExecuted)
      .map( r => r.computationResult.get )
      .withAttributes(supervisionStrategy(Supervision.resumingDecider))
      .runWith(storageSink)(streamMaterializer)
  }

  /**
   * Shuts down this component and frees all resources associated with it
   */
  def shutdown(): Unit = {
    mavenDownloader.shutdown()
    storageHandler.shutdown()

    streamMaterializer.shutdown()
    Await.ready(actorSystem.terminate(), 30.seconds)
  }


  private[extractor] def buildLibraryFeatureModel(libraryIdentifier: String): Future[IdempotentComputationResult[LibraryClassfileFeatureModel]] = Future {

    log.info(s"[$libraryIdentifier] Got identifier from queue")

    if(storageHandler.isLibraryPresent(libraryIdentifier)){
      log.warn(s"[$libraryIdentifier] Not processing identifier, as it is already in the database")
      IdempotentComputationResult.notExecuted[LibraryClassfileFeatureModel]()
    } else {
      getVersionsForLibrary(libraryIdentifier) match {
        case Success(versions) =>
          val libraryFeatureModel = new LibraryClassfileFeatureModel(libraryIdentifier, versions)

          Try {
            versions
              .map{ v => MavenIdentifier(libraryFeatureModel.groupId, libraryFeatureModel.artifactId, v) }
              .foreach{ identifier => processIdentifier(identifier, libraryFeatureModel)}
            opalProjectHelper.freeOpalResources()
          } match {
            case Success(_) =>
              log.info(s"[$libraryIdentifier] Successfully processed features")
              IdempotentComputationResult.success(libraryFeatureModel)
            case Failure(ex) =>
              log.error(s"[$libraryIdentifier] Failed to process versions of library", ex)
              IdempotentComputationResult.notSuccessful[LibraryClassfileFeatureModel](ex)
          }

        case Failure(ex) =>
          log.error(s"[$libraryIdentifier] Failed to generate version list", ex)
          IdempotentComputationResult.notSuccessful[LibraryClassfileFeatureModel](ex)
      }
    }
  }(streamMaterializer.executionContext)

  private[extractor] def processIdentifier(artifactIdentifier: MavenIdentifier,
                                           featureModel: LibraryClassfileFeatureModel): Unit = {
    mavenDownloader.downloadJar(artifactIdentifier) match {
      case MavenDownloadResult(identifier, Some(jarFile)) =>
        //TODO: We may also want to deal with dependencies here
        val classRepresentations = opalProjectHelper
          .readClassesFromJarStream(jarFile.is, jarFile.url, loadImplementation = true)
          .get // Exceptions will be handled by surrounding try in "buildLibraryFeatureModel"
          .map(t => toModel(t._1, handleFields = true))

        featureModel.appendNewRelease(identifier.version, classRepresentations)

      case MavenDownloadResult(identifier, None) =>
        log.warn(s"WARN: Swallowing failed JAR download for ${identifier.toString}")
        //TODO: Do we want to stop all computations for a single failed download?
    }
  }

  private[extractor] def buildStorageSink(): Sink[LibraryClassfileFeatureModel, Future[Done]] = {
    Sink.foreachAsync(configuration.numberOfStorageThreads){ libraryModel => Future(

      storageHandler.storeLibraryFeatureModel(libraryModel) match {
        case Success(_) =>
          log.info(s"[${libraryModel.libraryIdentifier}] Successfully stored library feature model")
        case Failure(ex) =>
          //IMPROVE: Maybe republish errors to queue?
          log.error(s"[${libraryModel.libraryIdentifier}] Failed to store library feature model", ex)
      }
    )(streamMaterializer.executionContext)}
  }

}
