package org.tud.cgcrawling.download

import akka.actor.ActorSystem
import akka.util.Timeout
import org.tud.cgcrawling.AppLogging
import org.tud.cgcrawling.discovery.maven.{JarFile, MavenIdentifier}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success}

class MavenJarDownloader(implicit val system: ActorSystem) extends AppLogging {

  private val downloader: HttpDownloader = new HttpDownloader()
  private val shutdownTimeout: Timeout = Timeout(30 seconds)

  def downloadJarFile(ident: MavenIdentifier): MavenJarDownloadResult = {
    downloader.downloadFromUri(ident.toJarLocation.toString) match {
      case Success(jar) =>
        log.info(s"Downloaded ${ident.toString}")
        MavenJarDownloadResult(ident, Some(JarFile(jar, ident.toJarLocation.toURL)))
      case Failure(ex) =>
        log.error(ex, s"Failed to download JAR file ${ident.toString}")
        MavenJarDownloadResult(ident, None)
    }
  }

  def shutdown(): Unit = {
    Await.ready(downloader.httpExt.shutdownAllConnectionPools(), shutdownTimeout.duration)
  }

}

case class MavenJarDownloadResult(identifier: MavenIdentifier, jarFile: Option[JarFile])