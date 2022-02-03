package org.tud.cgcrawling.model

import scala.collection.mutable

class ClassHierarchy private (val roots: Iterable[String]) {

  private val childMap = mutable.Map[String, Iterable[String]]()
  private val allTypes = mutable.HashSet[String]()

  def allTypeNames: Set[String] = allTypes.toSet

  def childrenOf(parent: String): Option[Iterable[String]] = childMap.get(parent)

  private def setChildren(parent: org.opalj.br.ObjectType, children: Iterable[org.opalj.br.ObjectType]): Unit =
    setChildren(parent.fqn, children.map(_.fqn))

  private def setChildren(parent: String, children: Iterable[String]): Unit = {
    allTypes.add(parent)
    children.foreach(allTypes.add)

    if(children.nonEmpty){
      childMap.put(parent, children)
    }
  }

}

object ClassHierarchy {

  def fromOPALModel(ch: org.opalj.br.ClassHierarchy): ClassHierarchy = {
    //rootTypes contains types and interfaces. Also "directSubtypesOf" retrieves both classes and interfaces
    val rootTypes = ch.rootTypes

    val theHierarchy = new ClassHierarchy(rootTypes.map(_.fqn))

    def addChildren(objType: org.opalj.br.ObjectType): Unit = {
      val children = ch.directSubtypesOf(objType).toIterable

      theHierarchy.setChildren(objType, children)

      children.foreach(addChildren)
    }

    rootTypes.foreach(addChildren)

    theHierarchy
  }

}
