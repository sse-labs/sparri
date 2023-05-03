package org.anon.spareuse.core.maven

import org.anon.spareuse.core.utils.http.HttpDownloader

import scala.util.Try

class MavenJarDownloader extends HttpDownloader {

  def downloadJar(identifier: MavenIdentifier): Try[MavenOnlineJar] = {
    downloadFromUri(identifier.toJarLocation.toString)
      .map(is => MavenOnlineJar(identifier, is, identifier.toJarLocation.toURL))
  }

}
