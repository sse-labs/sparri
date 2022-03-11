package de.tudo.classfilefeatures.common.maven.model

import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets

case class MavenIdentifier(groupId: String, artifactId: String, version: String, repository: String = "https://repo1.maven.org/maven2/") {

  def toUniqueString: String = repository + ":" + groupId + ":" + artifactId + ":" + version

  override val toString: String = groupId + ":" + artifactId + ":" + version

  def toJarLocation : URI = {
    constructArtifactBaseUri().resolve(encode(artifactId) + "-" + encode(version) + ".jar")
  }

  def toPomLocation : URI = {
    constructArtifactBaseUri().resolve(encode(artifactId) + "-" + encode(version) + ".pom")
  }

  def toLibraryString: String = groupId + ":" + artifactId

  private def constructArtifactBaseUri(): URI =
    new URI(repository)
      .resolve(encode(groupId).replace('.', '/') + "/")
      .resolve(encode(artifactId) + "/")
      .resolve(encode(version) + "/")

  private def encode(input : String) : String =
    URLEncoder.encode(input, StandardCharsets.UTF_8.toString)
}

object MavenIdentifier {

  val MavenCentralRepositoryURL = "https://repo1.maven.org/maven2/"
  private val DefaultRepository = MavenCentralRepositoryURL

  private implicit def wrapOption[A](value : A) : Option[A] = Some(value)

  def apply(s: String): Option[MavenIdentifier] = {
    if (!s.startsWith(DefaultRepository)) return None
    val identifier = s.replace(DefaultRepository + ":", "")
    val splitString: Array[String] = identifier.split(':')
    if (splitString.length < 2 || splitString.length > 3) return None

    MavenIdentifier(
      repository = DefaultRepository,
      groupId = splitString(0),
      artifactId = splitString(1),
      version = if (splitString.length < 3) "" else splitString(2))

  }
}
