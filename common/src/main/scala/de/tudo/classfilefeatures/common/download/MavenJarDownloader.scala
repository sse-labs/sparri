package de.tudo.classfilefeatures.common.download

import de.tudo.classfilefeatures.common.maven.model.MavenIdentifier
import org.slf4j.{Logger, LoggerFactory}

import scala.language.postfixOps
import scala.util.{Failure, Success}

class MavenJarDownloader {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val downloader: HttpDownloader = new HttpDownloader

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
    downloader.shutdown()
  }

}

case class MavenDownloadResult(identifier: MavenIdentifier, jarFile: Option[OnlineFile])