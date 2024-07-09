package org.anon.spareuse.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, OK}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import org.anon.spareuse.webapi.core.OracleResolutionRequestHandler
import org.anon.spareuse.webapi.core.OracleResolutionRequestHandler.{ClientOracleInteractionException, InvalidSessionException}
import org.anon.spareuse.webapi.model.oracle.{InitializeResolutionRequest, LookupResponse, OracleJsonSupport, StartResolutionRequest}
import org.slf4j.Logger
import spray.json.{JsObject, enrichAny}

import scala.util.{Failure, Success}

trait OracleRouteDefinitions extends OracleJsonSupport {

  protected val log: Logger

  protected val requestHandler: OracleResolutionRequestHandler


  protected def oracleRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("oracle"){
      path("start-session"){ post { startNewSession() }} ~
      path("resolve-entry"){ headerValueByName("session-id"){ sessionId => post { startResolving(sessionId) }}} ~
      path("pull-status"){ headerValueByName("session-id"){ sessionId => get { pullStatus(sessionId) }}} ~
      path("push-update"){ headerValueByName("session-id"){ sessionId => post { pushUpdate(sessionId) }}}
    }
  }

  private def startNewSession()(implicit request: HttpRequest): Route = entity(as[JsObject]){ e =>
    val initRequest = e.convertTo[InitializeResolutionRequest]
    log.debug(s"New Oracle resolution session requested by client.")

    onComplete(requestHandler.startResolutionSession(initRequest)) {
      case Success(session) =>
        //TODO: Most likely we want to make the initialization async and respond with the session id beforehand
        ???
      case Failure(ex) =>
        log.error(s"Internal error while starting new oracle session", ex)
        complete(InternalServerError)
    }

  }

  private def startResolving(sessionId: String)(implicit request: HttpRequest): Route = entity(as[JsObject]){ e =>
    val startRequest = e.convertTo[StartResolutionRequest]

    requestHandler.resolveFromEntrypoint(sessionId, startRequest) match {
      case Success(_) =>
        complete(OK)
      case Failure(isx: InvalidSessionException) =>
        log.warn(s"Invalid session Id provided: $sessionId", isx)
        complete(BadRequest, "Invalid session ID")
      case Failure(coix: ClientOracleInteractionException) =>
        log.warn(s"Failed to start resolution for session $sessionId due to bad request(s) by client", coix)
        complete(BadRequest, coix.getMessage)
      case Failure(ex) =>
        log.error(s"Failed to start resolution for session $sessionId due to unknown server error", ex)
        complete(InternalServerError)

    }
  }

  private def pullStatus(sessionId: String)(implicit request: HttpRequest): Route = {
    onComplete(requestHandler.pullLookupRequests(sessionId)){
      case Success(response) =>
        complete(response.toJson.compactPrint)
      case Failure(isx: InvalidSessionException) =>
        log.warn(s"Invalid session Id provided: $sessionId", isx)
        complete(BadRequest, "Invalid session ID")
      case Failure(coix: ClientOracleInteractionException) =>
        log.warn(s"Failed to pull status for session $sessionId due to bad request(s) by client", coix)
        complete(BadRequest, coix.getMessage)
      case Failure(ex) =>
        log.error(s"Failed to pull status for session $sessionId due to unknown server error", ex)
        complete(InternalServerError)
    }
  }

  private def pushUpdate(sessionId: String)(implicit request: HttpRequest): Route =  entity(as[JsObject]){ e =>
    val response = e.convertTo[LookupResponse]

    requestHandler.pushResponse(sessionId, response) match {
      case Success(_) =>
        complete(OK)
      case Failure(isx: InvalidSessionException) =>
        log.warn(s"Invalid session Id provided: $sessionId", isx)
        complete(BadRequest, "Invalid session ID")
      case Failure(coix: ClientOracleInteractionException) =>
        log.warn(s"Failed to push a summary for session $sessionId due to bad request(s) by client", coix)
        complete(BadRequest, coix.getMessage)
      case Failure(ex) =>
        log.error(s"Failed to push a summary for session $sessionId due to unknown server error", ex)
        complete(InternalServerError)
    }
  }

}
