package de.tudo.sse.spareuse.core.model.entities

import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind

import scala.collection.mutable

trait SoftwareEntityData {

  val name: String
  val language: String
  val kind: SoftwareEntityKind
  val repository: String

  def uid: String

  protected var parent: Option[SoftwareEntityData] = None
  protected var children: mutable.Set[SoftwareEntityData] = mutable.Set.empty

  def setChildren(c: Set[SoftwareEntityData]): Unit = {
    children = mutable.Set(c.toSeq: _*)
    children.foreach(_.parent = Some(this))
  }

  def addChild(c: SoftwareEntityData): Unit = {
    if (!children.contains(c)) children.add(c)
  }

  def getChildren: Set[SoftwareEntityData] = children.toSet

  def setParent(p: SoftwareEntityData): Unit = {
    parent = Some(p)
    p.addChild(this)
  }

  def hasParent: Boolean = parent.isDefined

  def getParent: Option[SoftwareEntityData] = parent

}
