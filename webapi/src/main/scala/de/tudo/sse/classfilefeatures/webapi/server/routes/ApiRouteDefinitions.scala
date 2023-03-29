package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, extractRequest, pathPrefix}
import akka.http.scaladsl.server.Route
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors

trait ApiRouteDefinitions extends EntityRouteDefinitions
  with ProcessingRouteDefinitions
  with AnalysisRouteDefinitions {


  protected lazy val allApiRoutes: Route = cors(){
    pathPrefix("api") {
      extractRequest { implicit request =>
        processingRoutes ~ entityRoutes ~ analysisRoutes
      }
    }
  }




}
