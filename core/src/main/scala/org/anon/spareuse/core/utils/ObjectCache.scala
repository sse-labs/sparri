package org.anon.spareuse.core.utils

import scala.collection.mutable

class ObjectCache[K <: AnyVal, T <: AnyRef](maxEntries: Int) {

  assert(maxEntries > 0)

  private val theCache: mutable.Map[K, (T, Long)] = new mutable.HashMap

  def hasValue(key: K): Boolean = theCache.contains(key)

  def pushValue(key: K, value: T): Unit = {

    theCache.put(key, (value, System.currentTimeMillis()))

    if(theCache.size > maxEntries) purgeEntries()
  }

  def getValueOpt(key: K): Option[T] = {
    theCache.get(key) match {
      case Some((obj, _)) =>
        theCache.update(key, (obj, System.currentTimeMillis()))
        Some(obj)
      case _ => None
    }
  }

  def getWithCache(key: K, valueCreator: () => T): T = {
    if(theCache.contains(key)) {
      getValueOpt(key).get
    } else {
      pushValue(key, valueCreator())
      theCache(key)._1
    }
  }

  private def purgeEntries(): Unit = {

    val keyNotUsedLongest = theCache
      .minBy(entry => entry._2._2)._1

    theCache.remove(keyNotUsedLongest)
  }
}
