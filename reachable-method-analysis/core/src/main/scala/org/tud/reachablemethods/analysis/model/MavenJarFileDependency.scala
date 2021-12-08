package org.tud.reachablemethods.analysis.model

import java.io.File

class MavenJarFileDependency(identifier: MavenIdentifier, jarFile: File, scope: Option[String]) {

  val classContainer: ClassContainerFile = ClassContainerFile(jarFile)
  val dependencyIdentifier: MavenIdentifier = identifier
  val dependencyScope: String = scope.getOrElse("default")


}
