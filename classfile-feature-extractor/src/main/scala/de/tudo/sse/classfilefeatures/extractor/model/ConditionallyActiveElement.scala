package de.tudo.sse.classfilefeatures.extractor.model

import scala.collection.mutable

trait ConditionallyActiveElement[T] {

  private val activeReleases: mutable.Set[String] = new mutable.HashSet

  val identifier: String

  def addActiveIn(release: String): Boolean = activeReleases.add(release)

  def isActiveIn(release: String): Boolean = activeReleases.contains(release)

  def allActiveReleases: Set[String] = activeReleases.toSet

  def appendActiveRelease(release: String, element: T): Unit = {
    if(!isActiveIn(release)){
      addActiveIn(release)

      updateModel(release, element)
    }
  }

  protected def updateModel(release: String, element: T): Unit

}
