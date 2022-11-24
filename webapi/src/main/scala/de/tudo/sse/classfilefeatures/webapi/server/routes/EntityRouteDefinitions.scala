package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, NotFound}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import spray.json.enrichAny

import scala.util.{Failure, Success}

trait EntityRouteDefinitions extends BasicRouteDefinition {

  /*-----------------------------------------
   |           ROUTE DECLARATIONS           |
   -----------------------------------------*/

  protected def entityRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("entities") {
      pathEnd { allEntitiesRoute() } ~
      pathPrefix(Segment) { entityName =>
        ensureEntityPresent(entityName) { singleEntityRelatedRoutes(entityName) }
      }
    }
  }

  private def allEntitiesRoute()(implicit request:HttpRequest): Route = get {
    // Support pagination via headers
    extractPaginationHeaders(request) { (limit, skip) =>

      // Support querying via query params (optional)
      parameters("kind".?, "parent".?, "language".?){ (kindOpt, parentOpt, languageOpt) =>
        //TODO: May want to make sure parent artifact is present if filtered for !?
        if(kindOpt.isDefined && SoftwareEntityKind.fromString(kindOpt.get).isEmpty) complete(BadRequest, s"Not a valid entity kind: ${kindOpt.get}")
        else allEntitiesRouteImpl(limit, skip, kindOpt.flatMap(SoftwareEntityKind.fromString), parentOpt, languageOpt)
      }
    }
  }

  private def singleEntityRelatedRoutes(entityName: String)(implicit request: HttpRequest): Route = {



    pathEnd { get { singleEntityRouteImpl(entityName) } } ~
    path("children") {
      get { extractPaginationHeaders(request){ (limit, skip) => allEntityChildrenRouteImpl(entityName, limit, skip) } }
    }~
    path("processedBy"){
      get { extractPaginationHeaders(request){ (limit, skip) => allAnalysisRunsForEntityRouteImpl(entityName, limit, skip) } }
    }~
    path("results"){
      get {
        extractPaginationHeaders(request) { (limit, skip) =>
          // Filter for analyses via query param
          parameters("analysis".?) { analysisOpt =>
            allResultsForEntityRouteImpl(entityName, limit, skip, analysisOpt)
          }
        }
      }

    }
  }




  private def allEntitiesRouteImpl(limit: Int, skip: Int, queriedKind: Option[SoftwareEntityKind], queriedParent: Option[String],
                                   queriedLanguage: Option[String])(implicit request: HttpRequest): Route = {

    if(queriedParent.isDefined && !requestHandler.hasEntity(queriedParent.get)) return complete(NotFound, s"Parent that was queried for not found: ${queriedParent.get}")
    if(limit <= 0) return complete(BadRequest, "Limit has to be greater than zero")

    log.debug(s"All entities requested (skip=$skip, limit=$limit). Filters: Kind=${queriedKind.getOrElse("None")}, Parent=${queriedParent.getOrElse("None")}, Language=${queriedLanguage.getOrElse("None")}")

    requestHandler.getAllEntities(limit, skip, queriedKind, queriedParent) match {
      case Success(entities) => complete(entities.toArray.toJson)
      case Failure(_) => complete(InternalServerError)
    }
  }

  private def singleEntityRouteImpl(entityName: String)(implicit request: HttpRequest): Route = {

  }

  private def allEntityChildrenRouteImpl(entityName: String, limit: Int, skip: Int)(implicit request: HttpRequest): Route = {

  }

  private def allAnalysisRunsForEntityRouteImpl(entityName: String, limit: Int, skip: Int)(implicit request: HttpRequest): Route = {

  }

  private def allResultsForEntityRouteImpl(entityName: String, limit: Int, skip: Int, queriedAnalysis: Option[String])(implicit request: HttpRequest): Route = {

  }

}
