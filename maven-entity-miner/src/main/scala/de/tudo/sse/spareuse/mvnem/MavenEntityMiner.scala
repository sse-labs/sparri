package de.tudo.sse.spareuse.mvnem

import akka.{Done, NotUsed}
import akka.stream.scaladsl.{Sink, Source}
import de.tudo.sse.spareuse.core.maven.{MavenIdentifier, MavenJarDownloader, MavenOnlineJar, MavenReleaseListDiscovery}
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaLibrary, JavaProgram}
import de.tudo.sse.spareuse.core.model.entities.conversion.OPALJavaConverter
import de.tudo.sse.spareuse.core.opal.OPALProjectHelper
import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqStreamIntegration
import de.tudo.sse.spareuse.core.utils.streaming.AsyncStreamWorker
import de.tudo.sse.spareuse.mvnem.storage.EntityMinerStorageAdapter
import de.tudo.sse.spareuse.mvnem.storage.impl.PostgresStorageAdapter

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class MavenEntityMiner(private[mvnem] val configuration: EntityMinerConfig)
    extends MqStreamIntegration
    with AsyncStreamWorker[String]
    with MavenReleaseListDiscovery {

  override val workerName: String = "maven-entity-miner"

  private final val downloader = new MavenJarDownloader()
  private final val opalProjectHelper = new OPALProjectHelper()
  private final val storageAdapter: EntityMinerStorageAdapter = new PostgresStorageAdapter()(streamMaterializer.executionContext)


  override def initialize(): Unit = {
    storageAdapter.initialize()
  }

  override def shutdown(): Unit = {
    downloader.shutdown()
    storageAdapter.shutdown()
    super.shutdown()
  }

  override protected def buildSource(): Source[String, NotUsed] = createMqMessageSource(configuration, abortOnEmptyQueue = false)

  override protected def buildStreamPipeline(source: Source[String, NotUsed]): Future[Done] = {
    source
      .flatMapConcat(msg => processMessage(msg))
      .filter(jarFileOpt => jarFileOpt.isDefined)
      .map(jarFileOpt => transform(jarFileOpt.get))
      .filter(storableOpt => storableOpt.isDefined)
      .map(storableOpt => storableOpt.get)
      .runWith(buildStorageSink())(streamMaterializer)
  }


  /**
   * Processes a message received from the queue into a JAR file with an open input stream ready for consumption. May return
   * None if the message was a remote control message, or an artifact with no associate JAR file.
   * @param message String message received from the queue
   * @return Either a JAR file representation, or None if the message was a remote control message
   */
  private def processMessage(message: String): Source[Option[MavenOnlineJar], NotUsed] = {

    // Very minimal remote control capabilities. Commands can be inserted via the MessageQueue (high prio msgs), need
    // to be prefixed with "[[MinerCommand]]". All other message will be interpreted as library names.
    val minerCommandPrefix = "[[MinerCommand]]"
    def isApplicationCommand(s: String): Boolean = s.startsWith(minerCommandPrefix)
    def getCommand(s: String): String = s.replace(minerCommandPrefix, "")

    if(isApplicationCommand(message)){

      getCommand(message).toLowerCase match {
        case "stop" => streamKillSwitch.shutdown()
        case cmd@_ =>
          log.warn(s"Unknown application command: $cmd")
      }

      Source.empty
    } else {

      messageToIdentifiers(message) match {
        case Some(identifiers) =>

          log.info(s"${identifiers.size} identifiers found for message $message")

          Source.fromIterator(() => identifiers.map{ ident =>
            downloader.downloadJar(ident) match {
              case Success(jarFile) => Some(jarFile)
              case Failure(HttpDownloadException(404, _, _)) =>
                // Do nothing when JAR does not exist
                None
              case Failure(ex) =>
                log.error(s"Failed to download JAR file for ${ident.toUniqueString}", ex)
                None
            }
          }.iterator)


        case None =>
          log.warn(s"No valid GA, GAV or UID message: $message")
          Source.empty
      }
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
  private def transform(jarFile: MavenOnlineJar): Option[JavaProgram] = {

    val start = System.currentTimeMillis()
    val classesTry = opalProjectHelper.readClassesFromJarStream(jarFile.content, jarFile.url, loadImplementation = true)
    val duration = System.currentTimeMillis() - start

    log.debug(s"OPAL init took $duration ms for ${jarFile.identifier.toString}")

    var result: Option[JavaProgram] = None

    classesTry match {
      case Success(classes) =>
        val programTry: Try[JavaProgram] =
          Try(OPALJavaConverter.convertProgram(jarFile.identifier.toString, "central", classes.map(_._1)))

        programTry match {
          case Success(programRep) =>
            programRep.setParent(new JavaLibrary(jarFile.identifier.toGA, "central"))
            log.info(s"Processing ${classes.size} classes in ${programRep.getChildren.size} packages @  ${jarFile.url.toString}")
            result = Some(programRep)
          case Failure(ex) =>
            log.error(s"Failed to extract program entities @ ${jarFile.url.toString}", ex)

        }

      case Failure(ex) =>
        log.error(s"Failed to process ${jarFile.identifier.toString}", ex)
    }

    jarFile.content.close()

    result
  }

  private def store(data: JavaProgram): Future[Unit] =
    storageAdapter.storeJavaProgram(data).map(_ => ())(streamMaterializer.executionContext)

  private[mvnem] def buildStorageSink(): Sink[JavaProgram, Future[Done]] = {
    Sink.foreachAsync(configuration.storageParallel) { data =>

      store(data).andThen {
        case Success(_) =>
          log.info(s"Successfully stored ${data.name}")
        case Failure(ex) =>
          log.error(s"Failed to store ${data.name}", ex)
      }(streamMaterializer.executionContext)

    }
  }


}
