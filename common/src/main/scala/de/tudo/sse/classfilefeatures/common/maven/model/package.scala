package de.tudo.sse.classfilefeatures.common.maven

import java.net.URI

package object model {
  final val MavenRepositoryUriString = "https://repo1.maven.org/maven2/"
  final val MavenRepositoryUri = new URI(MavenRepositoryUriString)
}
