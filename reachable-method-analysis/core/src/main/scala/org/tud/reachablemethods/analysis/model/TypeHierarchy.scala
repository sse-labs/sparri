package org.tud.reachablemethods.analysis.model

import org.tud.reachablemethods.analysis.logging.{AnalysisLogger, AnalysisLogging}

import scala.collection.mutable

class TypeHierarchy(override protected val log: AnalysisLogger) extends AnalysisLogging {

  private val typeParentMap: mutable.Map[String, Option[String]] = new mutable.HashMap()
  private val typeInterfacesMap: mutable.Map[String, mutable.Set[String]] = new mutable.HashMap()
  private val typeChildrenMap: mutable.Map[String, mutable.Set[String]] = new mutable.HashMap()

  /**
   * Sets the type parent in the map. Also updates children map if type parent is defined.
   * @param typeFqn FQN of the current type
   * @param typeParent Option containing the FQN of the parent type
   */
  private def setTypeParent(typeFqn: String, typeParent: Option[String]): Unit = {
    typeParentMap.put(typeFqn, typeParent)

    if(typeParent.isDefined){

      val parentFqn = typeParent.get

      // Always add update the children map accordingly
      if(!typeChildrenMap.contains(parentFqn)){
        typeChildrenMap.put(parentFqn, mutable.HashSet.empty)
      }
      typeChildrenMap(parentFqn).add(typeFqn)

      // We rely on the map to hold all types we know of, so add parent if it was not known before
      if(!typeParentMap.contains(parentFqn)){
        typeParentMap.put(parentFqn, None)
      }

      // Also create separate entry for current type in child map
      if(!typeChildrenMap.contains(typeFqn)){
        typeChildrenMap.put(typeFqn, mutable.HashSet.empty)
      }
    }
  }

  private def setTypeInterfaces(typeFqn: String, typeInterfaces: Iterable[String]): Unit = {
    // Make sure all interfaces are merged into the existing structure
    if(!typeInterfacesMap.contains(typeFqn)){
      typeInterfacesMap.put(typeFqn, mutable.Set.empty)
    }
    typeInterfaces.foreach(typeInterfacesMap(typeFqn).add)



    typeInterfaces.foreach { typeInterface =>

      // Make sure each parent interface gets its own entry in the map
      if(!typeInterfacesMap.contains(typeInterface)){
        typeInterfacesMap.put(typeInterface, mutable.HashSet.empty)
      }

      // Make sure the shared children map is updated for each interface
      if(!typeChildrenMap.contains(typeInterface)){
        typeChildrenMap.put(typeInterface, mutable.HashSet(typeFqn))
      } else {
        typeChildrenMap(typeInterface).add(typeFqn)
      }
    }


  }

  lazy val rootTypes: Set[TypeHierarchyNode] = {
    typeParentMap
      .filter( entry => entry._2.isEmpty)
      .map(entry => new TypeHierarchyNode(entry._1))
      .toSet
  }

  def allTypes: Set[TypeHierarchyNode] = (typeParentMap.keys.toSet ++ typeInterfacesMap.keys.toSet).map(new TypeHierarchyNode(_))

  def hasType(typeFqn: String): Boolean = typeParentMap.contains(typeFqn) || typeInterfacesMap.contains(typeFqn)

  def getType(typeFqn: String): Option[TypeHierarchyNode] = {
    if(hasType(typeFqn)) Some(new TypeHierarchyNode(typeFqn))
    else None
  }

  def addType(typeFqn: String, typeParent: Option[String], typeInterfaces: Iterable[String]): Unit = {

    // If we do not know the type, we accept the parent definition either way.
    // Also if we know the type but have an empty parent option, we accept the new parent option
    if(!typeParentMap.contains(typeFqn) || typeParentMap(typeFqn).isEmpty){
      setTypeParent(typeFqn, typeParent)
    }
    // If we know the type and have a parent definition which does not equal the new parent definition, we log an error
    else if(typeParent.isDefined && !typeParentMap(typeFqn).get.equals(typeParent.get)){
      log.warn(s"Tried to merge two different parents for type $typeFqn: ${typeParent.get} and ${typeParentMap(typeFqn).get}")
    }

    setTypeInterfaces(typeFqn, typeInterfaces)
  }

  def getParentOf(typeFqn: String): Option[TypeHierarchyNode] = {
    typeParentMap.get(typeFqn).flatten.map(new TypeHierarchyNode(_))
  }

  def getChildrenOf(typeFqn: String): Iterable[TypeHierarchyNode] = {
    typeChildrenMap
      .get(typeFqn)
      .map(list => list.toList.map(new TypeHierarchyNode(_)))
      .getOrElse(List.empty)
  }

  def getInterfacesOf(typeFqn: String): Iterable[TypeHierarchyNode] = {
    typeInterfacesMap
      .get(typeFqn)
      .map(list => list.toList.map(new TypeHierarchyNode(_)))
      .getOrElse(List.empty)
  }
}

class TypeHierarchyNode(val typeFqn: String) {

  def parent(implicit hierarchy: TypeHierarchy): Option[TypeHierarchyNode] = hierarchy.getParentOf(typeFqn)

  def children(implicit hierarchy: TypeHierarchy): Iterable[TypeHierarchyNode] = hierarchy.getChildrenOf(typeFqn)

  def interfaces(implicit hierarchy: TypeHierarchy): Iterable[TypeHierarchyNode] = hierarchy.getInterfacesOf(typeFqn)

  def allChildren(reflexive: Boolean = true)(implicit hierarchy: TypeHierarchy): Iterable[TypeHierarchyNode] = {

    val childList: mutable.Set[TypeHierarchyNode] = mutable.HashSet()

    def addChildrenRecursive(node: TypeHierarchyNode): Unit = {
      childList.add(node)

      node.children.foreach { child =>
        if(!childList.contains(child)){
          addChildrenRecursive(child)
        }
      }
    }

    children.foreach(addChildrenRecursive)

    if(reflexive){
      childList.add(this)
    }

    children.toSet
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: TypeHierarchyNode => typeFqn.equals(other.typeFqn)
    case _ => false
  }

  override def hashCode(): Int = typeFqn.hashCode
}
