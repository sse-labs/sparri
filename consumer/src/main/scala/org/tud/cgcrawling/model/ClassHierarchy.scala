package org.tud.cgcrawling.model

import scala.collection.mutable

class ClassHierarchy  {

  private val parentTypeMap = mutable.Map[String, Option[String]]()
  private val parentInterfacesMap = mutable.Map[String, Iterable[String]]()

  private val allTypes = mutable.HashSet[String]()

  def allTypeNames: Set[String] = allTypes.toSet

  def parentTypeOf(childFqn: String): Option[String] = parentTypeMap.get(childFqn).flatten
  def parentInterfacesOf(childFqn: String): Iterable[String] = parentInterfacesMap.getOrElse(childFqn, List.empty)

  private def setParentType(childType: org.opalj.br.ObjectType, parentType: Option[org.opalj.br.ObjectType]): Unit = {
    setParentType(childType.fqn, parentType.map(_.fqn))
  }

  private def setParentType(childFqn: String, parentFqn: Option[String]): Unit = {
    allTypes.add(childFqn)

    if(parentFqn.isDefined) {
      allTypes.add(parentFqn.get)
    }

    parentTypeMap.put(childFqn, parentFqn)
  }

  private def setParentInterfaces(childType: org.opalj.br.ObjectType, parentInterfaces: Iterable[org.opalj.br.ObjectType]): Unit = {
    setParentInterfaces(childType.fqn, parentInterfaces.map(_.fqn))

  }

  private def setParentInterfaces(childFqn: String, parentInterfaceFqns: Iterable[String]): Unit = {
    allTypes.add(childFqn)
    parentInterfaceFqns.foreach(allTypes.add)

    parentInterfacesMap.put(childFqn, parentInterfaceFqns)
  }

}

object ClassHierarchy {

  def fromOPALModel(ch: org.opalj.br.ClassHierarchy): ClassHierarchy = {
    val theHierarchy = new ClassHierarchy

    def addParents(objType: org.opalj.br.ObjectType): Unit = {
      theHierarchy.setParentType(objType, ch.superclassType(objType))
      theHierarchy.setParentInterfaces(objType, ch.superinterfaceTypes(objType).getOrElse(List.empty))
    }

    ch.foreachKnownType(addParents)

    theHierarchy
  }

}
