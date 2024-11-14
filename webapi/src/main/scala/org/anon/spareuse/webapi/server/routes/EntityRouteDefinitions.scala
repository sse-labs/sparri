package org.anon.spareuse.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, NotFound}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import spray.json.enrichAny

import scala.util.{Failure, Success, Try}

trait EntityRouteDefinitions extends BasicRouteDefinition {

  /*-----------------------------------------
   |           ROUTE DECLARATIONS           |
   -----------------------------------------*/

  protected def entityRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("entities") {
      pathEnd { allEntitiesRoute() } ~
      pathPrefix(Segment) { entityName =>
        ensureEntityPresent(entityName) { eid => singleEntityRelatedRoutes(eid) }
      }
    }
  }

  private def allEntitiesRoute()(implicit request:HttpRequest): Route = get {
    // Support pagination via headers
    extractPaginationHeaders(request) { (limit, skip) =>

      // Support querying via query params (optional)
      parameters("kind".?, "parent".?, "language".?){ (kindOpt, parentOpt, languageOpt) =>
        if(kindOpt.isDefined && SoftwareEntityKind.fromString(kindOpt.get).isEmpty) complete(BadRequest, s"Not a valid entity kind: ${kindOpt.get}")
        else allEntitiesRouteImpl(limit, skip, kindOpt.flatMap(SoftwareEntityKind.fromString), parentOpt, languageOpt)
      }
    }
  }

  private def singleEntityRelatedRoutes(entityId: Long)(implicit request: HttpRequest): Route = {



    pathEnd { get {
        parameters("depth".?){ depthOpt =>
          val depthIntOpt = depthOpt.flatMap(_.toIntOption)
          singleEntityRouteImpl(entityId, depthIntOpt)
        }
    } } ~
    path("children") {
      get { extractPaginationHeaders(request){ (limit, skip) => allEntityChildrenRouteImpl(entityId, limit, skip) } }
    }~
    path("processedBy"){
      get { extractPaginationHeaders(request){ (limit, skip) => allAnalysisRunsForEntityRouteImpl(entityId, limit, skip) } }
    }~
    path("results"){
      get {
        extractPaginationHeaders(request) { (limit, skip) =>
          // Filter for analyses via query param
          parameters("analysis".?) { analysisOpt =>
            allResultsForEntityRouteImpl(entityId, limit, skip, analysisOpt)
          }
        }
      }

    }
  }


  private def allEntitiesRouteImpl(limit: Int, skip: Int, queriedKind: Option[SoftwareEntityKind], queriedParent: Option[String],
                                   queriedLanguage: Option[String])(implicit request: HttpRequest): Route = {

    if(queriedParent.isDefined && queriedParent.get.toLongOption.isEmpty) return complete(BadRequest, s"Invalid ID for parent filter, must be Long-Value: ${queriedParent.get}")
    if(queriedParent.isDefined && !requestHandler.hasEntity(queriedParent.get.toLong)) return complete(NotFound, s"Parent that was queried for not found: ${queriedParent.get}")
    if(limit <= 0) return complete(BadRequest, "Limit has to be greater than zero")

    log.debug(s"All entities requested (skip=$skip, limit=$limit). Filters: Kind=${queriedKind.getOrElse("None")}, Parent=${queriedParent.getOrElse("None")}, Language=${queriedLanguage.getOrElse("None")}")

    onComplete(requestHandler.getAllEntities(limit, skip, queriedKind, queriedParent.map(_.toLong))) {
      case Success(entityReprs) => complete(entityReprs.toArray.toJson)
      case Failure(ex) =>
        log.error("Failure while retrieving list of entities", ex)
        complete(InternalServerError)
    }
  }

  private def singleEntityRouteImpl(eid:Long, depthOpt: Option[Int])(implicit request: HttpRequest): Route = {
    log.debug(s"Single entity requested, id=$eid, depth=$depthOpt.")


    if (depthOpt.isDefined && depthOpt.get < 0)
      complete(BadRequest, "Depth must be positive integer value")
    else
      onComplete(requestHandler.getEntity(eid, depthOpt)) {
        case Success(entity) => complete(entity.toJson)
        case Failure(ex) =>
          log.error("Failure while retrieving single entity information", ex)
          complete(InternalServerError)
      }
  }

  private def allEntityChildrenRouteImpl(eid: Long, limit: Int, skip: Int)(implicit request: HttpRequest): Route = {
    log.debug(s"Entity children requested, id=$eid (skip=$skip, limit=$limit) .")
    requestHandler.getEntityChildren(eid, skip, limit) match {
      case Success(childReps) =>
        complete(childReps.toJson)
      case Failure(ex) =>
        log.error("Failed to retrieve entity children", ex)
        complete(InternalServerError)

    }
  }

  private def allAnalysisRunsForEntityRouteImpl(eid: Long, limit: Int, skip: Int)(implicit request: HttpRequest): Route = {
    requestHandler.getAnalysisRunsForEntity(eid, limit, skip) match {
      case Success(runs) => complete(runs.toJson)
      case Failure(ex) =>
        log.error(s"Failed to retrieve analysis runs for entity $eid", ex)
        complete(InternalServerError)
    }
  }

  private def allResultsForEntityRouteImpl(eid: Long, limit: Int, skip: Int, queriedAnalysis: Option[String])(implicit request: HttpRequest): Route = {
    log.debug(s"Results for entity requested, id=$eid. Filters: Analysis=${queriedAnalysis.getOrElse("None")}")

    if(queriedAnalysis.isDefined){
      val queriedAnalysisStr = queriedAnalysis.get
      if(queriedAnalysisStr.isBlank) return complete(BadRequest, "Invalid analysis filter specified")
      else if(queriedAnalysisStr.split(":").length != 2) return complete(BadRequest, "Analysis name to filter for must be of format <name>:<version>")
      else {
        val parts = queriedAnalysisStr.split(":")
        if(!requestHandler.hasAnalysis(parts(0), Some(parts(1)))) return complete(NotFound, s"Analysis $queriedAnalysisStr not found in database")
      }
    }

    requestHandler.getAllResultsFor(eid, queriedAnalysis, limit, skip) match {
      case Success(result) => complete(result.toJson)
      case Failure(ex) =>
        log.error(s"Failure while retrieving results for entity $eid", ex)
        complete(InternalServerError)
    }
  }

}
