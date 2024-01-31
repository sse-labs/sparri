package org.anon.spareuse.mvnem

import akka.{Done, NotUsed}
import akka.stream.scaladsl.{Sink, Source}
import spray.json.{enrichAny, enrichString}
import org.anon.spareuse.core.maven.{MavenIdentifier, MavenJarDownloader, MavenOnlineJar, MavenReleaseListDiscovery}
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaLibrary, JavaProgram}
import org.anon.spareuse.core.model.entities.{MinerCommand, MinerCommandJsonSupport}
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.core.utils.http.HttpDownloadException
import org.anon.spareuse.core.utils.rabbitmq.{MqMessageWriter, MqStreamIntegration}
import org.anon.spareuse.core.utils.streaming.AsyncStreamWorker
import org.anon.spareuse.mvnem.storage.EntityMinerStorageAdapter
import org.anon.spareuse.mvnem.storage.impl.PostgresStorageAdapter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

class MavenEntityMiner(private[mvnem] val configuration: EntityMinerConfig)
    extends MqStreamIntegration
    with AsyncStreamWorker[String]
    with MavenReleaseListDiscovery
    with MinerCommandJsonSupport {

  override val workerName: String = "maven-entity-miner"

  private final val downloader = new MavenJarDownloader()
  private final val opalProjectHelper = new OPALProjectHelper()
  private final val storageAdapter: EntityMinerStorageAdapter = new PostgresStorageAdapter()(streamMaterializer.executionContext)

  private final val analysisQueueWriter = new MqMessageWriter(configuration.toWriterConfig)


  override def initialize(): Unit = {
    storageAdapter.initialize()
    analysisQueueWriter.initialize()
  }

  override def shutdown(): Unit = {
    downloader.shutdown()
    storageAdapter.shutdown()
    analysisQueueWriter.shutdown()
    super.shutdown()
  }

  override protected def buildSource(): Source[String, NotUsed] = createMqMessageSource(configuration.toReadConfig, abortOnEmptyQueue = false)

  override protected def buildStreamPipeline(source: Source[String, NotUsed]): Future[Done] = {
    source
      .map(getIdentifiersForMessage)
      .runWith(storeAllEntitiesSink)(streamMaterializer)
  }

  private def getIdentifiersForMessage(message: String): Set[MavenIdentifier] = {

    val command = Try(message.parseJson.convertTo[MinerCommand]) match {
      case Success(mc) => mc
      case Failure(_) => MinerCommand(Set(message), None) //This is convenience to support old queue format
    }

    val allIdentifiers = command.entityReferences.map(ref => (ref, messageToIdentifiers(ref))).flatMap {
      case (ref, Some(identifiers)) =>
        log.info(s"${identifiers.size} identifiers found for message $ref")
        identifiers
      case (ref, None) =>
        log.warn(s"No valid GA, GAV or UID reference: $ref")
        Set.empty
    }

    val allNewIdentifiers = allIdentifiers.filterNot(i => storageAdapter.hasProgram(i.toString))

    if(allIdentifiers.nonEmpty && allNewIdentifiers.isEmpty)
      log.info(s"All programs already indexed for message $message")

    allNewIdentifiers
  }

  def downloadJar(identifier: MavenIdentifier): Option[MavenOnlineJar] = {
    downloader.downloadJar(identifier) match {
      case Success(jarFile) => Some(jarFile)
      case Failure(HttpDownloadException(404, _, _)) =>
        // Do nothing when JAR does not exist
        None
      case Failure(ex) =>
        throw ex
    }
  }

  def storeAllEntitiesSink: Sink[Set[MavenIdentifier], Future[Done]] = {
    Sink.foreach { identifiers =>

      var cnt = 0
      var err = 0

      identifiers.foreach { identifier =>

        log.info(s"[$cnt/${identifiers.size}] Start processing ${identifier.toString} ... ")

        Try{
          downloadJar(identifier) match {
            case Some(jarFile) =>
              val representation = transform(jarFile)
              log.info(s"[$cnt/${identifiers.size}] Starting to store program for ${identifier.toString} ... ")
              storageAdapter.storeJavaProgram(representation).get
            case None =>
              // Do nothing, this is a GAV without a JAR
          }
        } match {
          case Success(_) =>
            log.info(s"[$cnt/${identifiers.size}] Done storing program for ${identifier.toString}.")
            cnt += 1
          case Failure(ex) =>
            log.error(s"[$cnt/${identifiers.size}] Failed to handle identifier ${identifier.toString}", ex)
            storageAdapter.ensureNotPresent(s"${identifier.toGA}!${identifier.toString}")
            cnt += 1
            err += 1
        }
      }

      log.info(s"Processed $cnt identifiers for message, got $err errors")
    }
  }

  /**
   * Method that extracts the program identifiers to process for a given message. The miner processes plain string messages,
   * and supports the following formats:
   *
   * - <G>:<A> => Entire library defined by groupd- and artifact id is processed
   * - <G>:<A>:<V> => Program defined by GAV triple is processed
   * - <GA>!<GAV>!<...> => Program containing the specified entity (GAV) is processed
   *
   * @param msg Message to process
   * @return A collection of identifiers to process, if the message has been valid. If not, None.
   */
  private def messageToIdentifiers(msg: String): Option[Iterable[MavenIdentifier]] = {

    implicit def toSingletonSeq(o: Option[MavenIdentifier]): Option[Iterable[MavenIdentifier]] = o.map(Seq(_))

    if(!msg.contains("!")){
      // Message is either a library (G:A) or a program (G:A:V)

      msg.count(_ == ':') match {
        case 1 =>
          getIdentifiersForLibrary(msg) match {
            case Success(identifiers) =>
              Some(identifiers)
            case Failure(ex) =>
              log.error("Failed to download version list", ex)
              None
          }
        case 2 =>
          MavenIdentifier.fromGAV(msg)
        case _ =>
          None
      }
    } else {
      // Message is a UID of format <GA>!<GAV>!<package>!...
      // We just need to extract the second entry <GAV>

      val parts = msg.split("!")

      if(parts.length < 2){
        None
      } else {
        MavenIdentifier.fromGAV(parts(1))
      }
    }
  }

  /**
   * Transforms a JAR file into a Software Entity Representation that can be stored afterwards.
   * @param jarFile JAR file with an open input stream
   * @return Data on Software Entity. Result will be of 'Program' Kind, and may have children that also need to be stored
   */
  private def transform(jarFile: MavenOnlineJar): JavaProgram = {

    val start = System.currentTimeMillis()
    val classesTry = opalProjectHelper.readClassesFromJarStream(jarFile.content, jarFile.url, loadImplementation = true)
    val duration = System.currentTimeMillis() - start

    log.debug(s"OPAL init took $duration ms for ${jarFile.identifier.toString}")

    var result: JavaProgram = null

    classesTry match {
      case Success(classes) =>
        val programTry: Try[JavaProgram] =
          Try(OPALJavaConverter.convertProgram(jarFile.identifier.toString, "central", classes.map(_._1)))

        programTry match {
          case Success(programRep) =>
            programRep.setParent(new JavaLibrary(jarFile.identifier.toGA, "central"))
            log.info(s"Processing ${classes.size} classes in ${programRep.getChildren.size} packages @  ${jarFile.url.toString}")
            result = programRep
          case Failure(ex) =>
            log.error(s"Failed to extract program entities @ ${jarFile.url.toString}", ex)
            throw ex
        }

      case Failure(ex) =>
        log.error(s"Failed to read classes from JAR file @ ${jarFile.identifier.toString}", ex)
        throw ex
    }

    jarFile.content.close()
    opalProjectHelper.freeOpalResources()

    result
  }



}
