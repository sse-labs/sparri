package org.anon.spareuse.webapi.server.routes

import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.model.StatusCodes.{Accepted, Conflict, Found, InternalServerError}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, complete, pathPrefix, post, respondWithHeaders}
import akka.http.scaladsl.server.Route
import org.anon.spareuse.webapi.model.requests.EnqueueRequest
import org.anon.spareuse.webapi.model.requests.EnqueueRequest

trait ProcessingRouteDefinitions extends BasicRouteDefinition {

  /*-----------------------------------------
   |           ROUTE DECLARATIONS           |
   -----------------------------------------*/

  protected def processingRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("processing") {
      pathPrefix("enqueueEntity") { triggerMinerForEntityRouteImpl }
    }
  }

  /*-----------------------------------------
   |           ROUTE IMPLEMENTATIONS        |
   -----------------------------------------*/

  private def triggerMinerForEntityRouteImpl(implicit request: HttpRequest): Route = post {
    entityAs[EnqueueRequest]{ entity =>

      log.debug(s"Entity indexing requested for id ${entity.Identifier}")

      if(requestHandler.hasEntity(entity.Identifier)) {
        val libUri = Uri(s"entities/${entity.Identifier}")
        respondWithHeaders(Location(libUri)) { complete(Found, s"Library ${entity.Identifier} has already been processed.")}
      } else {
        val enqueueSuccess = requestHandler.triggerEntityMining(entity.Identifier)
        if(enqueueSuccess) complete(Accepted)
        else complete(InternalServerError)
      }

    }
  }

}
