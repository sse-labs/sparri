package org.tud.cgcrawling.model

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeEvolution(val typeFqn: String) extends CgElementEvolution {

  private val instantiatedIn: mutable.ListBuffer[String] = new ListBuffer[String]
  private val childMap: mutable.Map[String, Iterable[String]] = new mutable.HashMap[String, Iterable[String]]


  private def activateRelease(release: String): Unit = if(isActiveIn.contains(release)) addActiveRelease(release)

  override def addActiveRelease(release: String): Unit = {
    // If type active in release, it is reasonable to assume it initially has no children in this release
    childMap.put(release, Iterable.empty)
    super.addActiveRelease(release)
  }

  def setChildrenIn(release: String, children: Iterable[String]): Unit = {
    activateRelease(release)
    childMap.update(release, children)
  }

  def setInstantiatedIn(release: String): Unit = {
    activateRelease(release)
    instantiatedIn.append(release)
  }

  def isInstantiatedIn: List[String] = instantiatedIn.toList

  def childFqnToReleasesMap: Map[String, Iterable[String]] = {

    val invertedMap = new mutable.HashMap[String, mutable.ListBuffer[String]]()
    for( (release, children) <- childMap) {
      children.foreach{ child =>

        if(!invertedMap.contains(child)){
          invertedMap.put(child, new ListBuffer[String])
        }

        invertedMap(child).append(release)
      }
    }

    invertedMap.mapValues(_.toIterable).toMap
  }

}
