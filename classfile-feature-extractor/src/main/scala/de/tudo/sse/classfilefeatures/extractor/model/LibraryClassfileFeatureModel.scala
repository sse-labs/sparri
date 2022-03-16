package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation

import scala.collection.mutable

class LibraryClassfileFeatureModel (val libraryIdentifier: String, val releases: Seq[String]) {

  val groupId: String = libraryIdentifier.split(":")(0)
  val artifactId: String = libraryIdentifier.split(":")(1)

  private val identifierToClassfileMap: mutable.Map[String, LibraryClassFileModel] = new mutable.HashMap

  def allClassfileModels: Seq[LibraryClassFileModel] = identifierToClassfileMap.values.toSeq

  def appendNewRelease(releaseVersion: String, classfileData: Seq[ClassFileRepresentation]): Unit = {

    classfileData.foreach{ cfr =>
      if(!identifierToClassfileMap.contains(cfr.thisTypeFqn)){
        identifierToClassfileMap.put(cfr.thisTypeFqn, new LibraryClassFileModel(cfr.thisTypeFqn))
      }

      identifierToClassfileMap(cfr.thisTypeFqn).appendActiveRelease(releaseVersion, cfr)
    }

  }

}
