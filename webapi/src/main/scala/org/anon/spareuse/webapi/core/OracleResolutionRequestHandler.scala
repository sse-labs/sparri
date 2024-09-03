package org.anon.spareuse.webapi.core

import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.execution.analyses.impl.cg.{InteractiveOracleAccessor, OracleCallGraphResolutionMode}
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.{LookupRequestRepresentation, OracleInteractionError}
import org.anon.spareuse.webapi.core.OracleResolutionRequestHandler.OracleState.OracleState
import org.anon.spareuse.webapi.core.OracleResolutionRequestHandler.{ClientOracleInteractionException, InvalidSessionException, OracleSessionState, OracleState}
import org.anon.spareuse.webapi.model.Session
import org.anon.spareuse.webapi.model.oracle.{InitializeResolutionRequest, LookupResponse, PullLookupRequestsResponse, StartResolutionRequest, toModel}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

class OracleResolutionRequestHandler(dataAccessor: DataAccessor)(implicit context: ExecutionContext) extends SessionBasedRequestHandler[OracleSessionState] {

  private[core] val sessionOracleAccessors: mutable.Map[String, InteractiveOracleAccessor] = mutable.Map.empty

  override protected[webapi] def initialSessionState: OracleSessionState = new OracleSessionState()


  /**
   * This method will create a new resolution session and initialize a fresh oracle accessor. Even if errors occur, the
   * session object will be returned, however it might already be invalidated base on the errors. Clients should validate
   * whether or not the session object contains an error.
   *
   * @param initRequest Request with all initialization data
   *
   * @return Future holding the session object - even in case of errors
   */
  def startResolutionSession(initRequest: InitializeResolutionRequest): Session[OracleSessionState] = {
    val theSession = newSession()
    val accessor = new InteractiveOracleAccessor(dataAccessor)
    sessionOracleAccessors.put(theSession.uid, accessor)

    def invalidateSession(): Unit = {
      theSession.sessionState.isValid = false
      sessionOracleAccessors.remove(theSession.uid)
      super.invalidateSession(theSession.uid)
    }

    Future{
      accessor.initialize(initRequest.libs, initRequest.types.map(toModel), initRequest.initTypes, initRequest.jreVersion,
        OracleCallGraphResolutionMode.fromId(initRequest.mode)) match {
        case Left(_) =>
          log.info(s"[${theSession.uid}] Successfully initialized accessor for session.")
          theSession.sessionState.currentState = OracleState.Initialized
        case Right(error) =>
          invalidateSession()
          theSession.sessionState.errors.addOne(error)
      }
    }.onComplete{
      case Failure(e: Exception) =>
        log.error(s"[${theSession.uid}] Failed to initialize accessor for session",e)
        invalidateSession()
      case _ =>
    }

    theSession
  }

  def resolveFromEntrypoint(sessionUid: String, startRequest: StartResolutionRequest): Try[Unit] = ensureValidSession(sessionUid) { session =>
    Try {
      if(!sessionOracleAccessors.contains(session.uid))
        throw new RuntimeException(s"Corrupt session state")

      if(session.getState.currentState != OracleState.Initialized)
        throw ClientOracleInteractionException(session, s"Accessor needs to be initialized and not busy to process entry points")

      sessionOracleAccessors(session.uid).startResolution(toModel(startRequest.cc), startRequest.ccPC, startRequest.types) match {
        case Left(_) =>
        case Right(error) =>
          log.warn(s"[$sessionUid] Cannot resolve from entrypoint: ${error.toString}")
          throw ClientOracleInteractionException(session, error.toString)
      }
    }
  }

  def pullLookupRequests(sessionUid: String): Future[PullLookupRequestsResponse] = ensureValidSessionF(sessionUid) { session =>
    Future {
      if(!sessionOracleAccessors.contains(session.uid))
        throw new RuntimeException(s"Corrupt session state")

      if(session.getState.currentState != OracleState.Initialized)
        throw ClientOracleInteractionException(session, s"Accessor needs to be initialized to check its state")

      val accessor = sessionOracleAccessors(session.uid)

      var req = accessor.nextRequest()
      val requests = mutable.ListBuffer.empty[LookupRequestRepresentation]

      while (req.nonEmpty) {
        requests.addOne(req.get)
        req = accessor.nextRequest()
      }

      if(accessor.succeeded){
        // We successfully processed the last entrypoint, so we are ready for another one (or finalization)
        PullLookupRequestsResponse(isResolving = false, requests = Set.empty, hasFailed = false, fatalError = None)
      } else if(accessor.failed){
        PullLookupRequestsResponse(isResolving = false, requests = Set.empty, hasFailed = true,
          fatalError = Some(accessor.firstFatalError.map(_.toString).getOrElse("<UNKNOWN>")))
      } else if(!accessor.isResolving){
        // We did not finish successfully, did not fail and are not working -> We are directly after initialization and
        // ready to process entrypoints
        PullLookupRequestsResponse(isResolving = false, requests = Set.empty, hasFailed = false, fatalError = None)
      } else {
        if(accessor.hasFatalErrors){
          // Accessor thinks we are working, but has encountered fatal errors
          PullLookupRequestsResponse(isResolving = false, requests = requests.toSet, hasFailed = true,
            fatalError = Some(accessor.firstFatalError.map(_.toString).getOrElse("<UNKNOWN>")))
        } else {
          // We are working - let client answer Lookup requests
          PullLookupRequestsResponse(isResolving = true, requests = requests.toSet, hasFailed = false, fatalError = None)
        }
      }

    }

  }

  def pushResponse(sessionUid: String, response: LookupResponse): Try[Unit] = ensureValidSession(sessionUid) { session =>
    if (!sessionOracleAccessors.contains(session.uid))
      throw new RuntimeException(s"Corrupt session state")

    if (session.getState.currentState != OracleState.Initialized)
      throw ClientOracleInteractionException(session, s"Accessor must be initialized in order to push results")

    Try {
      sessionOracleAccessors(session.uid).pushResponse(toModel(response))
    }
  }

  def finalize(sessionUid: String): Try[Any] = ensureValidSession(sessionUid){ session =>
    session.getState.currentState = OracleState.Finalized
    invalidateSession(sessionUid)
    Try { throw new RuntimeException("Not implemented")}
    //TODO:set new state, finalize summary generation
  }



  private def ensureValidSession[T](uid: String)(implicit func: Session[OracleSessionState] => Try[T]): Try[T] = {
    super.validateSessions()
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
    super.validateSessions()
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

    var currentState: OracleState = OracleState.NotInitialized

    var isValid: Boolean = true

    private[core] val errors = mutable.ListBuffer.empty[OracleInteractionError]

    def getErrors: Seq[OracleInteractionError] = errors.toSeq

  }

  object OracleState extends Enumeration {
    type OracleState = Value

    val NotInitialized: Value = Value(0) // Client needs to send initial information to initialize accessor
    val Initialized: Value = Value(1) // Accessor is ready for entrypoints to be uploaded to start processing
    val Finalized: Value = Value(2) // Accessor is finished and client confirmed that no more entrypoints are waiting for processing
  }

  case class InvalidSessionException(uid: String, reasonPhrase: String) extends Exception {
    override def getMessage: String = s"Invalid session with id $uid: $reasonPhrase"
  }

  case class ClientOracleInteractionException(session: Session[OracleSessionState], message: String) extends Exception {
    override def getMessage: String = s"[${session.uid}] $message"
  }

}
