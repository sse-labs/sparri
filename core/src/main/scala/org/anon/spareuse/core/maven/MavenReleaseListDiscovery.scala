package org.anon.spareuse.core.maven

import org.anon.spareuse.core.utils.http.HttpDownloader
import org.slf4j.{Logger, LoggerFactory}

import java.net.URI
import scala.util.{Failure, Success, Try}
import scala.xml.XML

/**
 * This trait can be mixed in to discover the list of Maven Artifacts for a given Library (GA-Tuple).
 */
trait MavenReleaseListDiscovery {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  protected val MavenRepoUri: URI = new URI(MavenIdentifier.DefaultRepository)

  def getVersionsForLibrary(libraryIdentifier: String): Try[Seq[String]] = {

    assert(libraryIdentifier.split(":").length == 2)

    val groupId = libraryIdentifier.split(":")(0)
    val artifactId = libraryIdentifier.split(":")(1)

    getVersions(groupId, artifactId).map(_.toSeq)
  }

  def getIdentifiersForLibrary(libraryIdentifier: String) : Try[Seq[MavenIdentifier]] = {
    val groupId = libraryIdentifier.split(":")(0)
    val artifactId = libraryIdentifier.split(":")(1)

    getVersionsForLibrary(libraryIdentifier).map( versions  => versions.map( v => new MavenIdentifier(MavenRepoUri.toString, groupId, artifactId, v)))
  }

  def createIdentifierIterator(groupId: String, artifactId: String): Try[Iterable[MavenIdentifier]] = {
    getVersions(groupId, artifactId)
      .map(versions => versions.map(version => new MavenIdentifier(MavenRepoUri.toString, groupId, artifactId, version)))
  }

  private def getVersions(groupId: String, artifactId: String): Try[Iterable[String]] = withHttpDownloader { downloader =>

    val versionListUri: URI = MavenRepoUri.resolve(relativeVersionListUrl(groupId, artifactId))

    downloader.downloadFromUri(versionListUri.toString) match {
      case Success(downloadResult) =>
        val versionListInputStream = downloadResult.contentStream
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
