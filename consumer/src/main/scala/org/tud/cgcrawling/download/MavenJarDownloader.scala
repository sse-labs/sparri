package org.tud.cgcrawling.download

import akka.actor.ActorSystem
import akka.util.Timeout
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.discovery.maven.{OnlineFile, MavenIdentifier}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success}

class MavenJarDownloader(implicit val system: ActorSystem) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val downloader: HttpDownloader = new HttpDownloader()
  private val shutdownTimeout: Timeout = Timeout(30 seconds)

  def downloadJar(ident: MavenIdentifier): MavenDownloadResult = {
      downloader.downloadFromUri(ident.toJarLocation.toString) match {
        case Success(jarStream) =>
          log.info(s"Downloaded ${ident.toString}")
          MavenDownloadResult(ident, Some(OnlineFile(jarStream, ident.toJarLocation.toURL)))
        case Failure(ex) =>
          log.error(s"Failed to download JAR file ${ident.toString}: ${ex.getMessage}")
          MavenDownloadResult(ident, None)
      }

  }

  def shutdown(): Unit = {
    Await.ready(downloader.httpExt.shutdownAllConnectionPools(), shutdownTimeout.duration)
  }

}

case class MavenDownloadResult(identifier: MavenIdentifier, jarFile: Option[OnlineFile])