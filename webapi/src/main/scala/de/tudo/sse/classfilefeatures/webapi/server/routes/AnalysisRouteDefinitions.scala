package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.StatusCodes.{Accepted, Found, InternalServerError, NotImplemented}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.server.Route
import de.tudo.sse.classfilefeatures.webapi.model.requests.ExecuteAnalysisRequest

import scala.util.{Failure, Success, Try}

trait AnalysisRouteDefinitions extends BasicRouteDefinition {



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
    pathEnd {
      get {
        extractPaginationHeaders(request){ (limit, skip) => allAnalysisRunsRouteImpl(analysisName, analysisVersion, limit, skip)}
      } ~ post {
        newAnalysisRunRouteImpl(analysisName, analysisVersion)
      }
    } ~
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

  private def allAnalysesRouteImpl(limit: Int, skip: Int)(implicit request:HttpRequest): Route = {

  }

  private def singleAnalysisRouteImpl(analysisName: String)(implicit request:HttpRequest): Route = {

  }

  private def singleAnalysisVersionRouteImpl(analysisName: String, analysisVersion: String)(implicit request:HttpRequest): Route = {

  }

  private def allAnalysisRunsRouteImpl(analysisName: String, analysisVersion: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {

  }

  private def newAnalysisRunRouteImpl(analysisName: String, analysisVersion: String)(implicit request: HttpRequest): Route = {
    entityAs[ExecuteAnalysisRequest]{ entity =>
      // Check if run for those inputs with this configuration exists
      val existingRunId: Option[String] = ???
      if(existingRunId.isDefined){
        val runUri = Uri(s"analyses/$analysisName/$analysisVersion/runs/${existingRunId.get}")
        respondWithHeaders(Location(runUri)){ complete(Found, "The analysis configuration requested has already been executed.")}
      } else {
        // Create new run, precompute id !!!
        val runId: Try[String]= ???

        runId match {
          case Success(id) => complete(Accepted, id)
          case Failure(ex) =>
            log.error("Failed to create a new run id for analysis request", ex)
            complete(InternalServerError)
        }
      }
    }
  }

  private def singleAnalysisRunRouteImpl(analysisName: String, analysisVersion: String, runId: String)(implicit request:HttpRequest): Route = {

  }

  private def allRunResultsRouteImpl(analysisName: String, analysisVersion: String, runId: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {

  }

  private def allRunInputsRouteImpl(analysisName: String, analysisVersion: String, runId: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {

  }

}
