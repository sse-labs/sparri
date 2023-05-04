package org.anon.spareuse.mvnpub.maven

import akka.NotUsed
import akka.stream.RestartSettings
import akka.stream.scaladsl.{RestartSource, Source}
import org.anon.spareuse.core.maven.MavenIdentifier
import org.apache.maven.index.reader.IndexReader
import org.slf4j.{Logger, LoggerFactory}

import java.net.{URI, URL}
import java.time.Duration
import java.util
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class MavenIndexReader(base: URL) {
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  log.info(s"New maven index reader create for $base")
  val ir = new IndexReader(null, new HttpResourceHandler(base.toURI.resolve(".index/")))

  log.info("Index Reader created")
  log.debug(ir.getIndexId)
  log.debug(ir.getPublishedTimestamp.toString)
  log.debug(ir.isIncremental.toString)
  log.debug(ir.getChunkNames.toString)

  lazy val cr = ir.iterator().next().iterator()

  def read(): Option[MavenIdentifier] = {

    def readInternal(kvp: util.Map[String, String]): Option[MavenIdentifier] = {

      if(!kvp.containsKey("u") && kvp.containsKey("del")){
        // In this case, the artifact was deleted. This is not an error, so don't attempt to process it
        None
      } else {
        val kvpU = kvp.get("u")
        val identifierAttempt = Try(kvpU.split("|".toCharArray))

        identifierAttempt match {
          case Success(identifier) => {
            val mavenId = MavenIdentifier(base.toString, identifier(0), identifier(1), identifier(2))

            Some(mavenId)
          }
          case Failure(e) => {
            log.warn(s"While processing index we received the following u-value that we could not split $kvpU. Exception was $e.")
            None
          }
        }
      }
    }

    cr.hasNext() match {
      case true => Try{
        Iterator.continually(readInternal(cr.next())).takeWhile(result => cr.hasNext()).collectFirst[MavenIdentifier]({ case Some(x) => x})
      } match {
        case Success(a) => a
        case Failure(ex) =>
          log.debug("Error", ex)
          None
      }
      case false =>
        log.debug("No more elements from index reader")
        None
    }
  }

  def close(): Unit = {
    ir.close()
  }
}

trait IndexProcessing {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def createSource(base: URI): Source[MavenIdentifier, NotUsed] = {
    log.info("Creating source")

    val sourceSettings = RestartSettings.create(minBackoff = Duration.ofSeconds(30),
      maxBackoff = Duration.ofSeconds(90), randomFactor = 0.2)
      .withMaxRestarts(5, 10.minutes)

    RestartSource.onFailuresWithBackoff(sourceSettings) {
      () => {
        val ir = Try(new MavenIndexReader(base.toURL))
        ir match {

          case Success(indexReader) =>
            Source.unfoldResource[MavenIdentifier, MavenIndexReader](
              () => indexReader,
              reader => reader.read(),
              reader => reader.close())

          case Failure(e) =>
            log.error(s"Could not reach resource. Terminating crawling for $base.")
            throw e

        }
      }
    }
  }

}