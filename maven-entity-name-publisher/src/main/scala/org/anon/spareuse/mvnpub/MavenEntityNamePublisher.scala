package org.anon.spareuse.mvnpub

import akka.{Done, NotUsed}
import akka.stream.scaladsl.{Sink, Source}
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.entities.{MinerCommand, MinerCommandJsonSupport}
import org.anon.spareuse.core.utils.rabbitmq.MqMessageWriter
import org.anon.spareuse.core.utils.streaming.AsyncStreamWorker
import org.anon.spareuse.mvnpub.maven.IndexProcessing
import spray.json.enrichAny

import java.net.URI
import scala.collection.mutable
import scala.concurrent.Future

class MavenEntityNamePublisher(config: MavenEntityPublisherConfig)
    extends IndexProcessing
    with AsyncStreamWorker[MavenIdentifier]
    with MinerCommandJsonSupport {

  private final val printoutStepping: Int = 5000

  private val mqWriter: MqMessageWriter = new MqMessageWriter(config.buildEntityQueueConfiguration)

  private val hashesSeen: mutable.Set[Int] = mutable.Set.empty

  override val workerName: String = "maven-name-publisher"

  override def initialize(): Unit = mqWriter.initialize()

  override protected def buildSource(): Source[MavenIdentifier, NotUsed] = createSource(new URI(config.mavenRepoBase))

  override protected def buildStreamPipeline(source: Source[MavenIdentifier, NotUsed]): Future[Done] = {
    source.runWith(Sink.foreach( handleIdentifier ))(streamMaterializer)
  }

  override def shutdown(): Unit = {
    log.info(s"Shutting down after publishing ${hashesSeen.size} unique entity names.")
    mqWriter.shutdown()
    super.shutdown()
  }

  private[mvnpub] def handleIdentifier(ident: MavenIdentifier): Unit = {

    val hash = ident.toString.hashCode

    if(!hashesSeen.contains(hash)){

      if(hashesSeen.size % printoutStepping == 0) {
        log.info(s"Published  ${hashesSeen.size} entity names so far..")
      }

      val command = MinerCommand(Set(ident.toString), None)

      mqWriter.appendToQueue(command.toJson.compactPrint, Some(1))

      hashesSeen.add(hash)
    }

  }

}
