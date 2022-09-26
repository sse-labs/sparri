package de.tudo.sse.spareuse.core.maven

import de.tudo.sse.spareuse.core.utils.http.HttpDownloader

import scala.util.Try

class MavenJarDownloader extends HttpDownloader {

  def downloadJar(identifier: MavenIdentifier): Try[MavenOnlineJar] = {
    downloadFromUri(identifier.toJarLocation.toString)
      .map(is => MavenOnlineJar(identifier, is, identifier.toJarLocation.toURL))
  }

}
