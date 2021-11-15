package org.tud.cgcrawling.callgraphs

import org.opalj.br.ClassFile
import org.tud.cgcrawling.discovery.maven.MavenIdentifier

import java.net.URL
import scala.collection.mutable

class ArtifactClassfileCache(maxCacheSize: Long) {

  private val theCache: mutable.Map[MavenIdentifier, List[(ClassFile, URL)]] = new mutable.HashMap()
  private val accessMap: mutable.Map[MavenIdentifier, Long] = new mutable.HashMap()

  def hasEntry(ident: MavenIdentifier): Boolean = theCache.contains(ident)

  def getEntry(ident: MavenIdentifier): Option[List[(ClassFile, URL)]] = {
    theCache.get(ident) match {
      case Some(value) =>
        accessMap.update(ident, System.currentTimeMillis())
        Some(value)
      case None => None
    }
  }

  def pushEntry(ident: MavenIdentifier, classes: List[(ClassFile, URL)]): List[(ClassFile, URL)] = {
    if(hasEntry(ident)){
      accessMap.update(ident, System.currentTimeMillis())
      getEntry(ident).get
    } else {
      if(theCache.size >= maxCacheSize){
        removeInternal()
      }
      theCache.put(ident, classes)
      accessMap.put(ident, System.currentTimeMillis())
      classes
    }
  }

  def clear(): Unit = {
    theCache.clear()
    accessMap.clear()
  }

  private def removeInternal(): Unit = {
    val identToRemove = accessMap.toList.minBy(_._2)._1
    theCache.remove(identToRemove)
    accessMap.remove(identToRemove)
  }

}
