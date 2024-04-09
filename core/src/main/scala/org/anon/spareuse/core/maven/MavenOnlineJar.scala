package org.anon.spareuse.core.maven

import java.io.InputStream
import java.net.URL

case class MavenOnlineJar(identifier: MavenIdentifier, content: InputStream, url: URL, timeOfUpload: String)
