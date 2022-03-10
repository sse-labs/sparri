package org.tud.reachablemethods.analysis.model

case class MavenIdentifier(groupId: String, artifactId: String, version: String){
  def libraryIdentifier: String = groupId + ":" + artifactId
  def coordinates: String = libraryIdentifier + ":" + version

  override def toString: String = coordinates
}
