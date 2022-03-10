package org.tud.cgcrawling.model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeEvolution(val typeFqn: String) extends CgElementEvolution {

  private val instantiatedIn: mutable.Set[String] = new mutable.HashSet[String]
  private val parentTypeMap: mutable.Map[String, Option[String]] = new mutable.HashMap[String, Option[String]]()
  private val parentInterfacesMap: mutable.Map[String, Iterable[String]] = new mutable.HashMap[String, Iterable[String]]()


  private def activateRelease(release: String): Unit = if(isActiveIn.contains(release)) addActiveRelease(release)

  override def addActiveRelease(release: String): Unit = {
    if(!parentTypeMap.contains(release)){
      parentTypeMap.put(release, None)
    }

    if(!parentInterfacesMap.contains(release)){
      parentInterfacesMap.put(release, mutable.Iterable.empty)
    }

    super.addActiveRelease(release)
  }

  def setParentTypeIn(release: String, parentType: Option[String]): Unit = {
    activateRelease(release)
    parentTypeMap.update(release, parentType)
  }

  def setParentInterfacesIn(release: String, parentInterfaces: Iterable[String]): Unit = {
    activateRelease(release)
    parentInterfacesMap.update(release, parentInterfaces)
  }

  def setInstantiatedIn(release: String): Unit = {
    activateRelease(release)
    instantiatedIn.add(release)
  }

  def isInstantiatedIn: Set[String] = instantiatedIn.toSet

  def parentTypeFqnToReleasesMap: Map[String, Iterable[String]] = {
    val invertedMap = new mutable.HashMap[String, mutable.Set[String]]()

    for( (release, parentOpt) <- parentTypeMap) {
      if(parentOpt.isDefined){
        if(!invertedMap.contains(parentOpt.get)){
          invertedMap.put(parentOpt.get, mutable.HashSet[String]())
        }
        invertedMap(parentOpt.get).add(release)
      }
    }

    invertedMap.mapValues(_.toIterable).toMap
  }

  def parentInterfaceFqnToReleasesMap: Map[String, Iterable[String]] = {
    val invertedMap = new mutable.HashMap[String, mutable.Set[String]]()

    for( (release, interfaces) <- parentInterfacesMap){
      interfaces.foreach{ interface =>

        if(!invertedMap.contains(interface)){
          invertedMap.put(interface, mutable.HashSet[String]())
        }

        invertedMap(interface).add(release)
      }
    }

    invertedMap.mapValues(_.toIterable).toMap
  }

}
