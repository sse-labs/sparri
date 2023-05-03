package org.anon.spareuse.core.maven

import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets

case class MavenIdentifier(repository: String, groupId: String, artifactId: String, version: String) {

  def toUniqueString: String = {
    repository + ":" + groupId + ":" + artifactId + ":" + version
  }


  override val toString: String = groupId + ":" + artifactId + ":" + version

  def toJarLocation : URI = {
    constructArtifactBaseUri().resolve(encode(artifactId) + "-" + encode(version) + ".jar")
  }

  def toPomLocation : URI = {
    constructArtifactBaseUri().resolve(encode(artifactId) + "-" + encode(version) + ".pom")
  }

  def toLibLocation : URI = {
    new URI(repository).resolve(encode(groupId).replace(".","/") + "/").resolve(encode(artifactId) + "/")
  }

  def toGA: String = groupId + ":" + artifactId

  private def constructArtifactBaseUri(): URI =
    new URI(repository)
      .resolve(encode(groupId).replace('.', '/') + "/")
      .resolve(encode(artifactId) + "/")
      .resolve(encode(version) + "/")

  private def encode(input : String) : String =
    URLEncoder.encode(input, StandardCharsets.UTF_8.toString)
}

object MavenIdentifier {
  val DefaultRepository = "https://repo1.maven.org/maven2/"

  def apply(s: String): Option[MavenIdentifier] = {
    if (!s.startsWith(DefaultRepository)) return None
    val identifier = s.replace(DefaultRepository + ":", "")
    val splitString: Array[String] = identifier.split(':')
    if (splitString.length < 2 || splitString.length > 3) return None

    Some(MavenIdentifier(
      repository = DefaultRepository,
      groupId = splitString(0),
      artifactId = splitString(1),
      version = if (splitString.length < 3) "" else splitString(2)))

  }

  def fromGAV(s: String): Option[MavenIdentifier] = {
    val parts = s.split(":")

    if(parts.length == 3){
      Some(MavenIdentifier(DefaultRepository, parts(0), parts(1), parts(2)))
    } else { None }
  }
}

