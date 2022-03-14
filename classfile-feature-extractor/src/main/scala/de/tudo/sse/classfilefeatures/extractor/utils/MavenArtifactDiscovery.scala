package de.tudo.sse.classfilefeatures.extractor.utils

import de.tudo.sse.classfilefeatures.common.download.HttpDownloader
import de.tudo.sse.classfilefeatures.common.maven.model.{MavenIdentifier, MavenRepositoryUri, MavenRepositoryUriString}
import org.slf4j.{Logger, LoggerFactory}

import java.net.URI
import scala.util.{Failure, Success, Try}
import scala.xml.XML

trait MavenArtifactDiscovery {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getVersionsForLibrary(libraryIdentifier: String): Try[Seq[String]] = {

    assert(libraryIdentifier.split(":").length == 2)

    val groupId = libraryIdentifier.split(":")(0)
    val artifactId = libraryIdentifier.split(":")(1)

    getVersions(groupId, artifactId).map(_.toSeq)
  }

  def createIdentifierIterator(groupId: String, artifactId: String): Try[Iterable[MavenIdentifier]] = {
    getVersions(groupId, artifactId)
      .map(versions => versions.map(version => new MavenIdentifier(groupId, artifactId, version, MavenRepositoryUriString)))
  }

  private def getVersions(groupId: String, artifactId: String): Try[Iterable[String]] = withHttpDownloader { downloader =>

    val versionListUri: URI = MavenRepositoryUri.resolve(relativeVersionListUrl(groupId, artifactId))

    downloader.downloadFromUri(versionListUri.toString) match {
      case Success(versionListInputStream) =>
        val xml = XML.load(versionListInputStream)
        versionListInputStream.close()

        val result = Try((xml \\ "metadata" \\ "versioning" \\ "versions" \\ "version").map(_.text))

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
