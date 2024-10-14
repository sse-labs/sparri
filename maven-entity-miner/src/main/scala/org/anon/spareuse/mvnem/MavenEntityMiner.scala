package org.anon.spareuse.mvnem

import akka.stream.OverflowStrategy
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

  private var downloader = new MavenJarDownloader()
  private var jarCnt = 0


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
      .map(messageToCommand)
      .runWith(storeAllSink)(streamMaterializer)
  }

  private def messageToCommand(message: String): MinerCommand = {
    Try(message.parseJson.convertTo[MinerCommand]) match {
      case Success(mc) => mc
      case Failure(_) => MinerCommand(Set(message), None) //This is convenience to support old queue format, in the future we may want to fail here
    }
  }

  private def getIdentifiersForCommand(command: MinerCommand): Set[MavenIdentifier] = {

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
      log.info(s"All programs already indexed for command ${command.toString}")

    allNewIdentifiers
  }

  def transformAndStore(minerCommand: MinerCommand): Unit = {
    minerCommand.entityReferences.foreach { entityRef =>
      Try {
        val programsToProcess = messageToIdentifiers(entityRef) match {
          case Some(identifiers) =>

            val newIdents = identifiers.filterNot(i => storageAdapter.hasProgram(i.toString))
            log.info(s"${newIdents.size} new identifiers found for message $entityRef")

            newIdents

          case None =>
            log.info(s"No identifiers for message $entityRef")
            Iterable.empty

        }

        programsToProcess.foreach{ identifier =>
          Try{
            downloadJar(identifier) match {
              case Some(jarFile) =>
                log.info(s"Start building index model for ${identifier.toString} ... ")
                val representation = transform(jarFile)
                log.info(s"Done building index model for ${identifier.toString}.")
                Await.result(storageAdapter.storeJavaProgram(representation), 15.minutes)
              case None =>
                log.info(s"No JAR present for $identifier")
            }
          } match {
            case Success(programName) =>
              log.info(s"Successfully stored index model for $programName")
            case Failure(ex) =>
              log.error(s"Failed to process ${identifier.toString}", ex)
          }
        }

      } match {
        case Success(_) =>
          log.info(s"Done processing all identifiers for $entityRef")
        case Failure(ex) =>
          log.error(s"Failed to enumerate identifiers for $entityRef", ex)
      }

    }

    if (minerCommand.analysisToTrigger.isDefined) {
      Try {
        analysisQueueWriter.appendToQueue(minerCommand.analysisToTrigger.get.toJson.compactPrint)
      } match {
        case Success(_) =>
          log.info(s"Successfully queued follow-up analysis after indexing finished")
        case Failure(ex) =>
          log.error(s"Failed to queue follow-up analysis for command ${minerCommand.toJson.compactPrint}", ex)
      }
    }


    opalProjectHelper.freeOpalResources()
  }


  def buildTransformAndStorageFuture(minerCommand: MinerCommand): Future[Set[String]] = {

    val identifiers = getIdentifiersForCommand(minerCommand)

    def buildTransformerFuture(identifier: MavenIdentifier): Future[Option[JavaProgram]] = Future {
      log.info(s"Start building index model for ${identifier.toString} ... ")

      Try {
        downloadJar(identifier).map { jarFile =>
          val representation = transform(jarFile)
          log.info(s"Done building index model for ${identifier.toString}.")
          representation
        }
      } match {
        case Success(repOpt) =>
          repOpt
        case Failure(ex) =>
          log.error(s"Failed to build index model for ${identifier.toString}", ex)
          None
      }
    }

    def buildStorageFuture(jpOpt: Option[JavaProgram]): Future[String] = {
      if (jpOpt.isDefined) {
        storageAdapter
          .storeJavaProgram(jpOpt.get)
          .andThen {
            case Success(jpName) =>
              log.info(s"Successfully stored index model for $jpName")
            case Failure(ex) =>
              log.error(s"Failed to store program.", ex)
          }
      } else {
        Future.successful("")
      }
    }


    val fullStorageFuture =
      if(identifiers.isEmpty)
        Future.successful(Set.empty[String])
      else if(identifiers.size == 1)
        buildTransformerFuture(identifiers.head)
          .flatMap(buildStorageFuture)
          .map(name => Set(name))
      else {
        var currentFuture: Future[Set[String]] = null
        identifiers.foreach { ident =>
          if(currentFuture == null) currentFuture = buildTransformerFuture(ident).flatMap(buildStorageFuture).map(name => Set(name))
          else currentFuture = currentFuture.flatMap { names =>
            buildTransformerFuture(ident).flatMap(buildStorageFuture).map(name => Set(name) ++ names)
          }
        }

        currentFuture
      }

    fullStorageFuture.andThen{ _ =>
      if (minerCommand.analysisToTrigger.isDefined) {
        Try {
          analysisQueueWriter.appendToQueue(minerCommand.analysisToTrigger.get.toJson.compactPrint)
        } match {
          case Success(_) =>
            log.info(s"Successfully queued follow-up analysis after indexing finished")
          case Failure(ex) =>
            log.error(s"Failed to queue follow-up analysis for command ${minerCommand.toJson.compactPrint}", ex)
        }
      }

      opalProjectHelper.freeOpalResources()
    }
  }

  def downloadJar(identifier: MavenIdentifier): Option[MavenOnlineJar] = {

    // Refresh downloader instance every so often to avoid deadlocks
    if(jarCnt >= 50){
      downloader = new MavenJarDownloader
      jarCnt = 0
    }

    jarCnt += 1

    downloader.downloadJar(identifier) match {
      case Success(jarFile) => Some(jarFile)
      case Failure(HttpDownloadException(404, _, _)) =>
        // Do nothing when JAR does not exist
        None
      case Failure(ex) =>
        throw ex
    }
  }

  val storeAllSink: Sink[MinerCommand, Future[Done]] = {
    Sink.foreach{ minerCommand =>
      transformAndStore(minerCommand)
    }
  }

  def storeAllEntitiesSink: Sink[MinerCommand, Future[Done]] = {
    Sink.foreachAsync(configuration.storageParallel) { minerCommand =>
      buildTransformAndStorageFuture(minerCommand)
        .map(_ => ())
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
          Try(OPALJavaConverter.convertProgram(jarFile.identifier.toString, "central", classes.map(_._1), jarFile.timeOfUpload))

        programTry match {
          case Success(programRep) =>
            programRep.setParent(new JavaLibrary(jarFile.identifier.toGA, "central", -1L))
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

    result
  }



}
