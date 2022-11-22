package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.server.Route

trait AnalysisRouteDefinitions extends BasicRouteDefinition {

  private implicit def todo[T]: Unit => T = ???

  protected def analysisRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("analyses"){
      pathEnd { allAnalysesRoute } ~
      pathPrefix(Segment){ analysisName =>
        ensureAnalysisPresent(analysisName){ singleAnalysisRelatedRoutes(analysisName) }
      }
    }
  }

  private def allAnalysesRoute(implicit request: HttpRequest): Route = get {
    extractPaginationHeaders(request, defaultLimit = 50) { (limit, skip) => allAnalysesRouteImpl(limit, skip) }
  }

  private def singleAnalysisRelatedRoutes(analysisName: String)(implicit request: HttpRequest): Route = {

    pathEnd{ get { singleAnalysisRouteImpl(analysisName) } } ~
    pathPrefix(Segment){ analysisVersion =>
      ensureAnalysisPresent(analysisName, analysisVersion){
        pathEnd { get { singleAnalysisVersionRouteImpl(analysisName, analysisVersion) } } ~
        pathPrefix("runs") { analysisRunRelatedRoutes(analysisName, analysisVersion) }
      }
    }
  }

  private def analysisRunRelatedRoutes(analysisName: String, analysisVersion: String)(implicit request: HttpRequest): Route = {
    pathEnd { get {
      extractPaginationHeaders(request){ (limit, skip) => allAnalysisRunsRouteImpl(analysisName, analysisVersion, limit, skip)}
    }} ~
    pathPrefix(Segment){ runId =>
      ensureAnalysisRunPresent(analysisName, analysisVersion, runId){
        pathEnd { get { singleAnalysisRunRouteImpl(analysisName, analysisVersion, runId) } }~
        path("inputs"){
          get { extractPaginationHeaders(request){ (limit, skip) => allRunInputsRouteImpl(analysisName, analysisVersion, runId, limit, skip) } }
        } ~
        path("results"){
          get { extractPaginationHeaders(request){ (limit, skip) => allRunResultsRouteImpl(analysisName, analysisVersion, runId, limit, skip) } }
        }
      }
    }
  }

  private def allAnalysesRouteImpl(limit: Int, skip: Int): Route = {

  }

  private def singleAnalysisRouteImpl(analysisName: String): Route = {

  }

  private def singleAnalysisVersionRouteImpl(analysisName: String, analysisVersion: String): Route = {

  }

  private def allAnalysisRunsRouteImpl(analysisName: String, analysisVersion: String, limit: Int, skip: Int): Route = {

  }

  private def singleAnalysisRunRouteImpl(analysisName: String, analysisVersion: String, runId: String): Route = {

  }

  private def allRunResultsRouteImpl(analysisName: String, analysisVersion: String, runId: String, limit: Int, skip: Int): Route = {

  }

  private def allRunInputsRouteImpl(analysisName: String, analysisVersion: String, runId: String, limit: Int, skip: Int): Route = {

  }

}
