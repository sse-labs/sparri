package org.anon.spareuse.webapi.core

import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.{LookupRequestRepresentation, OracleInteractionError}
import org.anon.spareuse.webapi.core.OracleResolutionRequestHandler.OracleSessionPhase.OracleSessionPhase
import org.anon.spareuse.webapi.core.OracleResolutionRequestHandler.{InvalidSessionException, OracleSessionPhase, OracleSessionState}
import org.anon.spareuse.webapi.model.Session

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

class OracleResolutionRequestHandler(dataAccessor: DataAccessor)(implicit context: ExecutionContext) extends SessionBasedRequestHandler[OracleSessionState] {

  private[core] val sessionOracleAccessors: mutable.Map[String, InteractiveOracleAccessor] = mutable.Map.empty

  override protected[webapi] def initialSessionState: OracleSessionState = new OracleSessionState()


  /**
   * This method will create a new resolution session and initialize a fresh oracle accessor. Even if errors occurr, the
   * session object will be returned, however it might already be invalidated base on the errors. Clients should validate
   * whether or not the session object contains an error.
   *
   * @return Future holding the session object - even in case of errors
   */
  def startResolutionSession(): Future[Session[OracleSessionState]] = {
    val theSession = newSession()

    def invalidateSession(): Unit = {
      theSession.sessionState.isValid = false
      sessionOracleAccessors.remove(theSession.uid)
      super.invalidateSession(theSession.uid)
    }

    Future{
      val accessor = new InteractiveOracleAccessor(dataAccessor)
      sessionOracleAccessors.put(theSession.uid, accessor)
      // TODO: Read in a suitable API model representation
      accessor.initialize(???,???,???,???, ???) match {
        case Left(_) =>
          log.info(s"[${theSession.uid}] Successfully initialized accessor for session.")
          theSession.sessionState.currentPhase = OracleSessionPhase.Initialized
          theSession
        case Right(error) =>
          invalidateSession()
          theSession.sessionState.errors.addOne(error)
          theSession
      }
    }.recover{
      case e: Exception =>
        log.error(s"[${theSession.uid}] Failed to initialize accessor for session",e)
        invalidateSession()
        theSession
    }

  }

  def resolveFromEntrypoint(sessionUid: String): Try[Unit] = ensureValidSession(sessionUid) { session =>
    Try {
      if(!sessionOracleAccessors.contains(session.uid))
        throw new RuntimeException(s"Corrupt session state")

      if(session.getState.currentPhase != OracleSessionPhase.Initialized)
        throw new RuntimeException(s"Accessor needs to be initialized and not busy to process entry points")

      // TODO: Read in a suitable API model representation
      sessionOracleAccessors(session.uid).startResolution(???, ???, ???) match {
        case Left(_) =>
          session.getState.currentPhase = OracleSessionPhase.ProcessingEntryPoint
        case Right(error) =>
          log.warn(s"[$sessionUid] Cannot resolve from entrypoint: ${error.toString}")
          throw new RuntimeException(error.toString)
      }
    }
  }

  def pullLookupRequests(sessionUid: String): Future[Seq[Any]] = ensureValidSessionF(sessionUid) { session =>
    Future {
      if(!sessionOracleAccessors.contains(session.uid))
        throw new RuntimeException(s"Corrupt session state")

      if(session.getState.currentPhase == OracleSessionPhase.NotInitialized)
        throw new RuntimeException(s"Accessor needs to be initialized to process entry points")

      val accessor = sessionOracleAccessors(session.uid)

      var req = accessor.nextRequest()
      val requests = mutable.ListBuffer.empty[LookupRequestRepresentation]

      while (req.nonEmpty) {
        requests.addOne(req.get)
        req = accessor.nextRequest()
      }

      if(accessor.succeeded){
        //TODO: Tell client that we are done
        ???
      } else if(accessor.failed){
        //TODO: Tell client that resolution failed, send errors.
        ???
      } else if(!accessor.isResolving){
        //TODO: Should not happen, this means that we are done resolving but with no definitive outcome!?
        ???
      } else {
        //TODO: This is the "real" branch where we are still resolving and waiting for the client to answer requests
        //TODO: Convert to a suitable API model representation
        requests.toSeq
      }

    }

  }

  def pushResponse(sessionUid: String): Try[Unit] = ensureValidSession(sessionUid) { session =>
    if (!sessionOracleAccessors.contains(session.uid))
      throw new RuntimeException(s"Corrupt session state")

    if (session.getState.currentPhase != OracleSessionPhase.ProcessingEntryPoint)
      throw new RuntimeException(s"Accessor must be processing an entrypoint in order to push results")

    Try {
      // TODO: Defined suitable API model representation for response
      sessionOracleAccessors(session.uid).pushResponse(???)
    }
  }

  def finalize(sessionUid: String): Try[Any] = {
    ???
    //TODO: Validate session, set new state, finalize summary generation
  }



  private def ensureValidSession[T](uid: String)(implicit func: Session[OracleSessionState] => Try[T]): Try[T] = {
    if(isActiveSession(uid)){
      func(getSession(uid).get)
    } else if(isTimedOut(uid)) {
      Failure(InvalidSessionException(uid, "Session timed out"))
    } else if(isInvalid(uid)){
      Failure(InvalidSessionException(uid, "Session was invalidated due to errors"))
    } else {
      Failure(InvalidSessionException(uid, "Session ID unknown to server"))
    }
  }

  private def ensureValidSessionF[T](uid: String)(implicit func: Session[OracleSessionState] => Future[T]): Future[T] = {
    if (isActiveSession(uid)) {
      func(getSession(uid).get)
    } else if (isTimedOut(uid)) {
      Future.failed(InvalidSessionException(uid, "Session timed out"))
    } else if (isInvalid(uid)) {
      Future.failed(InvalidSessionException(uid, "Session was invalidated due to errors"))
    } else {
      Future.failed(InvalidSessionException(uid, "Session ID unknown to server"))
    }
  }


}

object OracleResolutionRequestHandler {
  class OracleSessionState private[core]() {

    var currentPhase: OracleSessionPhase = OracleSessionPhase.NotInitialized

    var isValid: Boolean = true

    private[core] val errors = mutable.ListBuffer.empty[OracleInteractionError]

    def getErrors: Seq[OracleInteractionError] = errors.toSeq

  }

  object OracleSessionPhase extends Enumeration {
    type OracleSessionPhase = Value

    val NotInitialized: Value = Value(0) // Client needs to send initial information to initialize accessor
    val Initialized: Value = Value(1) // Accessor is ready for entrypoints to be uploaded to start processing
    val ProcessingEntryPoint: Value = Value(2) // Accessor is currently working on processing an entrypoint
    val Finished: Value = Value(3) // Accessor is finished and client confirmed that no more entrypoints are waiting for processing
  }

  case class InvalidSessionException(uid: String, reasonPhrase: String) extends Exception {
    override def getMessage: String = s"Invalid session with id $uid: $reasonPhrase"
  }

}
