package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Directives.{_enhanceRouteWithConcatenation, pathEnd, pathPrefix, get}
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.server.Route

trait AnalysisRouteDefinitions extends BasicRouteDefinition {

  protected def analysisRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("analyses"){
      pathEnd { allAnalysesRoute } ~
      pathPrefix(Segment){ analysisName =>
        ensureAnalysisPresent(analysisName){ singleAnalysisRelatedRoutes(analysisName) }
      }
    }
  }

  protected def singleAnalysisRelatedRoutes(analysisName: String)(implicit request: HttpRequest): Route = {
    pathEnd { singleAnalysisRoute(analysisName)} ~
    pathPrefix("results") { singleAnalysisResultsRoute(analysisName) } ~
    pathPrefix("processed") { singleAnalysisProcessedEntitiesRoute(analysisName) }
  }

  private def allAnalysesRoute(implicit request: HttpRequest): Route = get {
    extractPaginationHeaders(request, defaultLimit = 50){ (limit: Int, skip: Int) =>
      __TODO__completeNotImplemented
    }
  }

  private def singleAnalysisRoute(analysisName: String)(implicit request: HttpRequest): Route = get {
    __TODO__completeNotImplemented
  }

  private def singleAnalysisResultsRoute(analysisName: String)(implicit request: HttpRequest): Route = get {
    __TODO__completeNotImplemented
  }

  private def singleAnalysisProcessedEntitiesRoute(analysisName: String)(implicit request: HttpRequest): Route = get {
    __TODO__completeNotImplemented
  }

}
