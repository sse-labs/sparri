package org.tud.cgcrawling.model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait CgElementEvolution {
  private val activeIn: mutable.ListBuffer[String] = new ListBuffer[String]

  def addActiveRelease(release: String): Unit = {
    if(!activeIn.contains(release)) activeIn.append(release)
  }

  def isActiveIn: List[String] = activeIn.toList
}
