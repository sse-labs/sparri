package org.anon.spareuse.webapi.core

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}

class SimpleValueCache[T <: AnyVal](entryTtl: Duration = 120.seconds, minPurgeCycle: Duration = 30.seconds) {

  private var lastPurge: Long = System.currentTimeMillis()

  private val theCache: mutable.Map[String, (T, Long)] = new mutable.HashMap

  def pushValue(ident: String, value: T): Unit = theCache.put(ident, (value, System.currentTimeMillis()))

  def getValueOpt(ident: String): Option[T] = {
    purgeEntries()

    theCache.get(ident).map(_._1)
  }

  def getWithCache(ident: String, valueCreator: () => T): T = {
    purgeEntries()

    if(theCache.contains(ident)) {
      theCache(ident)._1
    } else {
      pushValue(ident, valueCreator())
      theCache(ident)._1
    }
  }

  private def purgeEntries(): Unit = {

    val millisSinceLastPurge = System.currentTimeMillis() - lastPurge

    if(millisSinceLastPurge > minPurgeCycle.toMillis){
      val identifierToPurge = theCache.flatMap { entry =>
        val key = entry._1
        val createdAt = entry._2._2

        val keyDuration = System.currentTimeMillis() - createdAt

        if(keyDuration > entryTtl.toMillis) Some(key)
        else None
      }

      identifierToPurge.foreach(theCache.remove)

      lastPurge = System.currentTimeMillis()
    }

  }

}
