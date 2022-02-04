package org.tud.cgcrawling.model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeEvolution(val typeFqn: String) extends CgElementEvolution {

  private val instantiatedIn: mutable.ListBuffer[String] = new ListBuffer[String]
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
    instantiatedIn.append(release)
  }

  def isInstantiatedIn: List[String] = instantiatedIn.toList

  def parentTypeFqnToReleasesMap: Map[String, Iterable[String]] = {
    val invertedMap = new mutable.HashMap[String, mutable.ListBuffer[String]]()

    for( (release, parentOpt) <- parentTypeMap) {
      if(parentOpt.isDefined){
        if(!invertedMap.contains(parentOpt.get)){
          invertedMap.put(parentOpt.get, new ListBuffer[String])
        }
        invertedMap(parentOpt.get).append(release)
      }
    }

    invertedMap.mapValues(_.toIterable).toMap
  }

  def parentInterfaceFqnToReleasesMap: Map[String, Iterable[String]] = {
    val invertedMap = new mutable.HashMap[String, mutable.ListBuffer[String]]()

    for( (release, interfaces) <- parentInterfacesMap){
      interfaces.foreach{ interface =>

        if(!invertedMap.contains(interface)){
          invertedMap.put(interface, new ListBuffer[String])
        }

        invertedMap(interface).append(release)
      }
    }

    invertedMap.mapValues(_.toIterable).toMap
  }

}
