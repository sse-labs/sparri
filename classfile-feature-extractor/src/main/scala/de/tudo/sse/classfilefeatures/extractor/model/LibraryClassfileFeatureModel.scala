package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation

class LibraryClassfileFeatureModel (val libraryIdentifier: String, val releases: Seq[String]) {

  val groupId: String = libraryIdentifier.split(":")(0)
  val artifactId: String = libraryIdentifier.split(":")(1)

  def appendNewRelease(releaseVersion: String, classfileData: Seq[ClassFileRepresentation]): Unit = {
    //TODO: IMPLEMENT
  }

}
