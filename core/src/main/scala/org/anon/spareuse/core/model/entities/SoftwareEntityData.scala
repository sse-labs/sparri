package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind

import scala.annotation.tailrec
import scala.collection.mutable

trait SoftwareEntityData {

  val name: String
  val language: String
  val kind: SoftwareEntityKind
  val repository: String

  val binaryHash: Option[Array[Byte]]

  val uid: String

  protected var parent: Option[SoftwareEntityData] = None
  protected var children: mutable.Set[SoftwareEntityData] = mutable.Set.empty

  def setChildren(c: Set[SoftwareEntityData]): Unit = {
    children = mutable.Set(c.toSeq: _*)
    children.foreach(_.parent = Some(this))
  }

  def addChild(c: SoftwareEntityData): Unit = {
    if (!children.contains(c)){
      c.parent = Some(this)
      children.add(c)
    }
  }

  def getChildren: Set[SoftwareEntityData] = children.toSet

  def setParent(p: SoftwareEntityData): Unit = {
    parent = Some(p)
    p.addChild(this)
  }

  def hasParent: Boolean = parent.isDefined

  def getParent: Option[SoftwareEntityData] = parent

  def isLibrary: Boolean = kind == SoftwareEntityKind.Library
  def isProgram: Boolean = kind == SoftwareEntityKind.Program
  def isPackage: Boolean = kind == SoftwareEntityKind.Package
  def isClass: Boolean = kind == SoftwareEntityKind.Class
  def isMethod: Boolean = kind == SoftwareEntityKind.Method
  def isFieldAccessInstruction: Boolean = kind == SoftwareEntityKind.FieldAccessStatement
  def isMethodInvocationInstruction: Boolean = kind == SoftwareEntityKind.InvocationStatement
  def isInstruction: Boolean = isFieldAccessInstruction || isMethodInvocationInstruction

  @tailrec
  final def findFirstParent(pred: SoftwareEntityData => Boolean): Option[SoftwareEntityData] = {
    if(pred(this)) Some(this)
    else if(!hasParent) None
    else getParent.get.findFirstParent(pred)
  }

}
