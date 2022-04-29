package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.model.StatusCodes.{Accepted, Conflict, InternalServerError}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives.{complete, pathPrefix, post, respondWithHeaders}
import akka.http.scaladsl.server.Route
import de.tudo.sse.classfilefeatures.webapi.model.requests.EnqueueRequest

trait ProcessingRouteDefinitions extends BasicRouteDefinition {

  /*-----------------------------------------
   |           ROUTE DECLARATIONS           |
   -----------------------------------------*/

  protected def processingRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("processing") {
      pathPrefix("enqueuePackage") { enqueuePackageRoute }
    }
  }

  /*-----------------------------------------
   |           ROUTE IMPLEMENTATIONS        |
   -----------------------------------------*/

  private def enqueuePackageRoute(implicit request: HttpRequest): Route = post {
    entityAs[EnqueueRequest]{ entity =>

      if(requestHandler.hasLibrary(entity.packageName)) {
        // Do not enqueue packages which are already fully processed => respond with 409 Conflict
        val packageUri = Uri(s"packages/${entity.packageName}")
        respondWithHeaders(Location(packageUri)) { complete(Conflict, s"Package ${entity.packageName} has already been processed.") }
      } else {
        val enqueueSuccess = requestHandler.processEnqueueLibraryRequest(entity.packageName)
        if(!enqueueSuccess) complete(InternalServerError)
        else complete(Accepted)
      }
    }
  }

}
