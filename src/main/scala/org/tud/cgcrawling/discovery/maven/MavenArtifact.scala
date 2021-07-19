package org.tud.cgcrawling.discovery.maven

import org.joda.time.DateTime

import java.io.InputStream
import java.net.URL

case class JarFile(is: InputStream, url: URL)

case class PomFile(is: InputStream)


case class MavenArtifact(identifier : MavenIdentifier, jarFile: Option[JarFile], pomFile: PomFile, publicationDate: Option[DateTime])
