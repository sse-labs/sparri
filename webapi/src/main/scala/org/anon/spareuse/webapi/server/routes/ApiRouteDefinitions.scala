package org.anon.spareuse.webapi.server.routes

import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, extractRequest, pathPrefix}
import akka.http.scaladsl.server.Route
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors

trait ApiRouteDefinitions extends EntityRouteDefinitions
  with ProcessingRouteDefinitions
  with AnalysisRouteDefinitions
  with OracleRouteDefinitions {


  protected lazy val allApiRoutes: Route = cors(){
    pathPrefix("api") {
      extractRequest { implicit request =>
        processingRoutes ~ entityRoutes ~ analysisRoutes ~ oracleRoutes
      }
    }
  }




}
