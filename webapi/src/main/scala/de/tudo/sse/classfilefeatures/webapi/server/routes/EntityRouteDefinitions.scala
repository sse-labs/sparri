package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind

trait EntityRouteDefinitions extends BasicRouteDefinition {

  implicit def todo[T]: Unit => T = ???

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
                                   queriedLanguage: Option[String]): Route = {

  }

  private def singleEntityRouteImpl(entityName: String): Route = {

  }

  private def allEntityChildrenRouteImpl(entityName: String, limit: Int, skip: Int): Route = {

  }

  private def allAnalysisRunsForEntityRouteImpl(entityName: String, limit: Int, skip: Int): Route = {

  }

  private def allResultsForEntityRouteImpl(entityName: String, limit: Int, skip: Int, queriedAnalysis: Option[String]): Route = {

  }

}
