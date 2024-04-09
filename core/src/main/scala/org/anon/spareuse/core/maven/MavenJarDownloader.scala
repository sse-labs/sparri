package org.anon.spareuse.core.maven

import org.anon.spareuse.core.utils.http.HttpDownloader

import scala.util.{Failure, Success, Try}

class MavenJarDownloader extends HttpDownloader {

  private final val LastModifiedHeaderName: String = "Last-Modified"

  def downloadJar(identifier: MavenIdentifier): Try[MavenOnlineJar] = {
    downloadFromUri(identifier.toJarLocation.toString, headersToRead = List(LastModifiedHeaderName)) match {
      case Success(streamWithHeaders) =>
        Success(MavenOnlineJar(identifier, streamWithHeaders.contentStream, identifier.toJarLocation.toURL, streamWithHeaders.headers.getOrElse(LastModifiedHeaderName, "<NONE>")))
      case Failure(ex) => Failure(ex)
    }

  }

}
