package org.tud.cgcrawling.discovery.maven

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.util.Timeout
import org.tud.cgcrawling.AppLogging
import org.tud.cgcrawling.download.{HttpDownloader, HttpException}

import java.net.URI
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.xml.XML

trait LibraryArtifactProcessing extends AppLogging {

  private val shutdownTimeout: Timeout = new Timeout(20 seconds)

  val repoUri: URI

  def createIdentifierSource(groupId: String, artifactId: String)
                            (implicit system: ActorSystem): Try[Source[MavenIdentifier, NotUsed]] = {
    val sourceTry = getVersions(groupId, artifactId)
      .map(versions =>
        versions.map(version => new MavenIdentifier(repoUri.toString, groupId, artifactId, version)))
      .map(it => Source.fromIterator(() => it.iterator))

    sourceTry
  }

  private def getVersions(groupId: String, artifactId: String)
                         (implicit system: ActorSystem): Try[Iterable[String]] = withHttpDownloader(system) { downloader =>

    val versionListUri: URI = repoUri.resolve(relativeVersionListUrl(groupId, artifactId))

    downloader.downloadFromUri(versionListUri.toString) match {
      case Success(versionListInputStream) =>
        val xml = XML.load(versionListInputStream)
        versionListInputStream.close()

        val result = Try((xml \\ "metadata" \\ "versioning" \\ "versions" \\ "version").map(_.text).toList)

        result
      case Failure(x: HttpException) =>
        log.error( s"Failed to download version list with code ${x.code}")
        Failure(x)
      case Failure(ex) =>
        log.error(ex, "Failed to read version list")
        Failure(ex)
    }
  }

  private def withHttpDownloader[T](system: ActorSystem)(implicit function: HttpDownloader => T): T = {
    val downloader = new HttpDownloader()(system)

    val result: T = function.apply(downloader)

    Await.ready(downloader.httpExt.shutdownAllConnectionPools(), shutdownTimeout.duration)

    result
  }

  private def relativeVersionListUrl(groupId: String, artifactId: String): String = {
    s"${groupId.replace(".", "/")}/$artifactId/maven-metadata.xml"
  }

}
