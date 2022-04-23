package de.tudo.sse.classfilefeatures.webapi.core

import scala.collection.mutable

class ObjectCache[T <: AnyRef](maxEntries: Int) {

  assert(maxEntries > 0)

  private val theCache: mutable.Map[String, (T, Long)] = new mutable.HashMap


  def pushValue(ident: String, value: T): Unit = {

    theCache.put(ident, (value, System.currentTimeMillis()))

    if(theCache.size > maxEntries) purgeEntries()
  }

  def getValueOpt(ident: String): Option[T] = {
    theCache.get(ident) match {
      case Some((obj, _)) =>
        theCache.update(ident, (obj, System.currentTimeMillis()))
        Some(obj)
      case _ => None
    }
  }

  def getWithCache(ident: String, valueCreator: () => T): T = {
    if(theCache.contains(ident)) {
      getValueOpt(ident).get
    } else {
      pushValue(ident, valueCreator())
      theCache(ident)._1
    }
  }

  private def purgeEntries(): Unit = {

    val keyNotUsedLongest = theCache
      .minBy(entry => entry._2._2)._1

    theCache.remove(keyNotUsedLongest)
  }
}
