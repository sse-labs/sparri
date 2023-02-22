package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.StatusCodes.{Accepted, Found, InternalServerError, OK}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.server.Route
import de.tudo.sse.classfilefeatures.webapi.model.requests.ExecuteAnalysisRequest
import spray.json.enrichAny

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
          get { extractPaginationHeaders(request){ (limit, skip) => allRunResultsRouteImpl(runId, limit, skip) } }
        }
      }
    }
  }

  private def allAnalysesRouteImpl(limit: Int, skip: Int)(implicit request:HttpRequest): Route = {
    requestHandler.getAnalyses(limit, skip) match {
      case Success(analyses) =>
        complete(analyses.toJson)
      case Failure(ex) =>
        log.error("Failed to return list of analyses", ex)
        complete(InternalServerError)
    }
  }

  private def singleAnalysisRouteImpl(analysisName: String)(implicit request:HttpRequest): Route = {

  }

  private def singleAnalysisVersionRouteImpl(analysisName: String, analysisVersion: String)(implicit request:HttpRequest): Route = {

  }

  private def allAnalysisRunsRouteImpl(analysisName: String, analysisVersion: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {

  }

  private def newAnalysisRunRouteImpl(analysisName: String, analysisVersion: String)(implicit request: HttpRequest): Route = {

    def buildRunUri(runId: String): Uri = Uri(s"analyses/$analysisName/$analysisVersion/runs/$runId")


    entityAs[ExecuteAnalysisRequest]{ entity =>
      // Check if run for those inputs with this configuration exists
      requestHandler.getRunIdIfPresent(analysisName, analysisVersion, entity) match {
        // If it exists, respond with "Found" and location of run result
        case Success(Some(id)) =>
          respondWithHeaders(Location(buildRunUri(id))){ complete(Found, "The analysis configuration requested has already been executed.")}

        // If it does not exist, create new run record and trigger analysis. Respond with id of new run record
        case Success(None) =>
          val runId: Try[String] = requestHandler.triggerNewAnalysisRun(analysisName, analysisVersion, entity)
          runId match {
            case Success(id) => respondWithHeaders(Location(buildRunUri(id))) {
              complete(Accepted, id)
            }
            case Failure(ex) =>
              log.error("Failed to create a new run id for analysis request", ex)
              complete(InternalServerError)
          }

        // If checking DB for entries failed, respond with error
        case Failure(_) =>
          complete(InternalServerError)
      }
    }
  }

  private def singleAnalysisRunRouteImpl(analysisName: String, analysisVersion: String, runId: String)(implicit request:HttpRequest): Route = {
    // Can be sure identifying triple of name, version and run is present!
    requestHandler.getRun(analysisName, analysisVersion, runId) match {
      case Success(repr) =>
        complete(OK, repr.toJson)
      case Failure(_) =>
        complete(InternalServerError)
    }
  }

  private def allRunResultsRouteImpl(runId: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {
    requestHandler.getRunResults(runId, limit, skip) match {
      case Success(results) =>
        complete(results.toJson)
      case Failure(ex) =>
        log.error(s"Failed to retrieve results for run $runId", ex)
        complete(InternalServerError)
    }
  }

  private def allRunInputsRouteImpl(analysisName: String, analysisVersion: String, runId: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {

  }

}
