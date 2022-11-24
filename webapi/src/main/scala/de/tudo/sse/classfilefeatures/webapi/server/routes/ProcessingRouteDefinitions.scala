package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.model.StatusCodes.{Accepted, Conflict, Found, InternalServerError}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, complete, pathPrefix, post, respondWithHeaders}
import akka.http.scaladsl.server.Route
import de.tudo.sse.classfilefeatures.webapi.model.requests.EnqueueRequest

trait ProcessingRouteDefinitions extends BasicRouteDefinition {

  /*-----------------------------------------
   |           ROUTE DECLARATIONS           |
   -----------------------------------------*/

  protected def processingRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("processing") {
      pathPrefix("enqueueProgram") { triggerMinerForProgramRouteImpl } ~
      pathPrefix("enqueueLibrary") { triggerMinerForLibraryRouteImpl }
    }
  }

  /*-----------------------------------------
   |           ROUTE IMPLEMENTATIONS        |
   -----------------------------------------*/

  private def triggerMinerForProgramRouteImpl(implicit request: HttpRequest): Route = post {
    entityAs[EnqueueRequest]{ entity =>

      if(requestHandler.hasLibrary(entity.Identifier)) {
        val libUri = Uri(s"entities/${entity.Identifier}")
        respondWithHeaders(Location(libUri)) { complete(Found, s"Library ${entity.Identifier} has already been processed.")}
      } else {
        val enqueueSuccess = ???
        if(enqueueSuccess) complete(Accepted)
        else complete(InternalServerError)
      }

    }
  }

  private def triggerMinerForLibraryRouteImpl(implicit request: HttpRequest): Route = post {
    entityAs[EnqueueRequest]{ entity =>
      if (???) {
        val libUri = Uri(s"entities/${entity.Identifier}")
        respondWithHeaders(Location(libUri)) {
          complete(Found, s"Program ${entity.Identifier} has already been processed.")
        }
      } else {
        val enqueueSuccess = ???
        if (enqueueSuccess) complete(Accepted)
        else complete(InternalServerError)
      }
    }
  }

}
