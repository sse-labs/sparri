package org.tud.cgcrawling.discovery.maven

import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.download.HttpDownloader

import java.net.URI
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.xml.XML

trait LibraryArtifactProcessing {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  val repoUri: URI


  def createIdentifierIterator(groupId: String, artifactId: String): Try[Iterable[MavenIdentifier]] = {
    getVersions(groupId, artifactId)
      .map(versions => versions.map(version => new MavenIdentifier(repoUri.toString, groupId, artifactId, version)))
  }

  private def getVersions(groupId: String, artifactId: String): Try[Iterable[String]] = withHttpDownloader { downloader =>

    val versionListUri: URI = repoUri.resolve(relativeVersionListUrl(groupId, artifactId))

    downloader.downloadFromUri(versionListUri.toString) match {
      case Success(versionListInputStream) =>
        val xml = XML.load(versionListInputStream)
        versionListInputStream.close()

        val result = Try((xml \\ "metadata" \\ "versioning" \\ "versions" \\ "version").map(_.text).take(3)) //TODO: REVERT!

        result
      case Failure(ex) =>
        log.error("Failed to read version list", ex)
        Failure(ex)
    }
  }

  private def withHttpDownloader[T](implicit function: HttpDownloader => T): T = {
    val downloader = new HttpDownloader

    val result: T = function.apply(downloader)

    downloader.shutdown()

    result
  }

  private def relativeVersionListUrl(groupId: String, artifactId: String): String = {
    s"${groupId.replace(".", "/")}/$artifactId/maven-metadata.xml"
  }

}
