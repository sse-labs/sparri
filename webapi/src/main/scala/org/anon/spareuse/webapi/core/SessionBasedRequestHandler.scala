package org.anon.spareuse.webapi.core

import org.anon.spareuse.webapi.model.Session
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

trait SessionBasedRequestHandler[T] {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  private[webapi] val sessionDict: mutable.Map[String, Session[T]] = mutable.HashMap.empty
  private[webapi] val timedOutSessionIds: mutable.Set[String] = mutable.HashSet.empty
  private[webapi] val invalidSessionIds: mutable.Set[String] = mutable.HashSet.empty

  protected[webapi] def getSession(uid: String): Option[Session[T]] = sessionDict.get(uid).filter(_.isActive)
  protected[webapi] def removeSession(uid: String): Unit = sessionDict.remove(uid)
  protected[webapi] def invalidateSession(uid: String): Unit = {
    if(sessionDict.contains(uid)) sessionDict(uid).isActive = false
    removeSession(uid)
    invalidSessionIds.add(uid)
  }

  protected[webapi] def isTimedOut(uid: String): Boolean = timedOutSessionIds.contains(uid)
  protected[webapi] def isInvalid(uid: String): Boolean = invalidSessionIds.contains(uid)
  protected[webapi] def isActiveSession(uid: String): Boolean = sessionDict.contains(uid) && sessionDict(uid).isActive

  protected[webapi] def initialSessionState: T

  protected[webapi] def newSession(): Session[T] = {
    var uid = Session.newUid

    while(sessionDict.contains(uid) || timedOutSessionIds.contains(uid)) { uid = Session.newUid }

    val session = Session.newSession(uid, initialSessionState)

    sessionDict.put(uid, session)

    session
  }

  protected[webapi] def validateSessions(): Unit = {
    for(key: String <- sessionDict.keys.toSet){
      if(sessionDict(key).isTimedOut){
        sessionDict.remove(key)
        timedOutSessionIds.add(key)
      }
    }
  }

}
