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

      log.debug(s"Entity indexing requested for ${entity.Identifiers.length} ids.")

      val identifiersToProcess = entity.Identifiers.filter(requestHandler.validIndexEntity)

      if(identifiersToProcess.isEmpty) {
        complete(Found, s"All requested entities are already indexed.")
      } else {
        val enqueueSuccess = requestHandler.triggerEntityMining(identifiersToProcess.toIndexedSeq)
        if(enqueueSuccess) complete(Accepted)
        else complete(InternalServerError)
      }

    }
  }

}
