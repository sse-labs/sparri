package org.tud.cgcrawling.model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CgElementEvolution {
  private val activeIn: mutable.Set[String] = new mutable.HashSet[String]

  def addActiveRelease(release: String): Unit = {
    if(!activeIn.contains(release)) activeIn.add(release)
  }

  def isActiveIn: Set[String] = activeIn.toSet
}
