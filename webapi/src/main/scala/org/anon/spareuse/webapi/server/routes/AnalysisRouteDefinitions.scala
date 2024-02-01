package org.anon.spareuse.webapi.server.routes

import akka.http.scaladsl.model.StatusCodes.{Accepted, BadRequest, Found, InternalServerError, OK}
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.server.Route
import org.anon.spareuse.webapi.model.requests.ExecuteAnalysisRequest
import org.anon.spareuse.webapi.model.requests.ExecuteAnalysisRequest
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
        pathPrefix("runs") { analysisRunRelatedRoutes(analysisName, analysisVersion) } ~
        path("result-format") { get { analysisResultFormatRouteImpl(analysisName, analysisVersion) } }
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
          get { allRunInputsRouteImpl(analysisName, analysisVersion, runId) }
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

  private def analysisResultFormatRouteImpl(analysisName: String, analysisVersion: String): Route = {
    requestHandler.getAnalysisResultFormat(analysisName, analysisVersion) match {
      case Success(resultFormat) =>
        complete(resultFormat.toJson)
      case Failure(ex) =>
        log.error(s"Failed to retrieve analysis result format for $analysisName:$analysisVersion", ex)
        complete(InternalServerError)
    }
  }

  private def singleAnalysisRouteImpl(analysisName: String)(implicit request:HttpRequest): Route = {
    requestHandler.getAnalyses(analysisName) match {
      case Success(analyses) =>
        complete(analyses.toJson)
      case Failure(ex) =>
        log.error(s"Failed to get list of analyses for $analysisName", ex)
        complete(InternalServerError)
    }
  }

  private def singleAnalysisVersionRouteImpl(analysisName: String, analysisVersion: String)(implicit request:HttpRequest): Route = {
    requestHandler.getAnalysis(analysisName, analysisVersion) match {
      case Success(analysisData) =>
        complete(analysisData.toJson)
      case Failure(ex) =>
        log.error(s"Failed to retrieve analysis information for $analysisName:$analysisVersion", ex)
        complete(InternalServerError)
    }
  }

  private def allAnalysisRunsRouteImpl(analysisName: String, analysisVersion: String, limit: Int, skip: Int)(implicit request:HttpRequest): Route = {
    requestHandler.getAnalysisRuns(analysisName, analysisVersion, limit, skip) match {
      case Success(runData) =>
        complete(runData.toJson)
      case Failure(ex) =>
        log.error(s"Failed to retrieve analysis runs for $analysisName:$analysisVersion", ex)
        complete(InternalServerError)
    }
  }

  private def newAnalysisRunRouteImpl(analysisName: String, analysisVersion: String)(implicit request: HttpRequest): Route = {

    def buildRunUri(runId: String): Uri = Uri(s"analyses/$analysisName/$analysisVersion/runs/$runId")


    entityAs[ExecuteAnalysisRequest]{ entity =>

      log.debug(s"New analysis run requested, name=$analysisName, version=$analysisVersion, #inputs=${entity.Inputs.length}, user=${entity.User.getOrElse("None")}")

      // Check if run for those inputs with this configuration exists
      requestHandler.getRunIdIfPresent(analysisName, analysisVersion, entity) match {
        // If it exists, respond with "Found" and location of run result
        case Success(Some(id)) =>
          respondWithHeaders(Location(buildRunUri(id))){ complete(Found, "The analysis configuration requested has already been executed.")}

        // If it does not exist, create new run record and trigger analysis. Respond with id of new run record
        case Success(None) =>

          requestHandler.validateRunRequest(analysisName, analysisVersion, entity) match {

            // If consistency checks for request fail, respond with appropriate error message
            case (false, msg) =>
              complete(BadRequest, msg)

            // If request is valid, proceed with handling it
            case (true, _) =>
              val runId: Try[String] = requestHandler.triggerNewAnalysisRun(analysisName, analysisVersion, entity)
              runId match {
                case Success(id) => respondWithHeaders(Location(buildRunUri(id))) {
                  complete(Accepted, id)
                }
                case Failure(ex) =>
                  log.error("Failed to create a new run id for analysis request", ex)
                  complete(InternalServerError)
              }
          }

        // If checking DB for entries failed, respond with error
        case Failure(_) =>
          complete(InternalServerError)
      }
    }
  }

  private def singleAnalysisRunRouteImpl(analysisName: String, analysisVersion: String, runId: String)(implicit request:HttpRequest): Route = {

    log.debug(s"Single analysis run requested, name=$analysisName, version=$analysisVersion, runId=$runId")

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

  private def allRunInputsRouteImpl(analysisName: String, analysisVersion: String, runId: String)(implicit request:HttpRequest): Route = {
    requestHandler.getRun(analysisName, analysisVersion, runId).map(_.Inputs) match {
      case Success(runInputs) => complete(runInputs.toJson)
      case Failure(ex) =>
        log.error(s"Failed to retrieve inputs for analysis run $runId", ex)
        complete(InternalServerError)
    }
  }

}
