package org.anon.spareuse.webapi.model

import akka.http.scaladsl.model.DateTime
import org.anon.spareuse.webapi.model.Session.DefaultSessionTimeout

import java.util.UUID
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class Session[T] private(ident: String, createdAt: DateTime, state: T) {

  private[webapi] val sessionState = state
  private[webapi] var lastRequest = createdAt
  private[webapi] var isActive: Boolean = true

  val uid: String = ident
  val creationTime: DateTime = createdAt

  def getState: T = sessionState

  def recordInteraction(): Unit = {
    if(!isTimedOut){
      lastRequest = DateTime.now
    }
  }

  def isTimedOut: Boolean = {
    if(isActive) isActive = DateTime.now.clicks - lastRequest.clicks <= DefaultSessionTimeout.toMillis
    !isActive
  }


}

object Session {

  val DefaultSessionTimeout: FiniteDuration = 60.seconds

  def newSession[T](uid: String, sessionState: T, createdAt: DateTime = DateTime.now): Session[T] =
    new Session(uid, createdAt, sessionState)

  def newUid: String = UUID.randomUUID().toString

}
