package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import spray.json.enrichAny

trait PackageRouteDefinitions extends BasicRouteDefinition {

  /*-----------------------------------------
   |           ROUTE DECLARATIONS           |
   -----------------------------------------*/

  protected def packageRoutes(implicit request: HttpRequest): Route = {
    pathPrefix("packages") {
      pathEnd { allPackagesRoute } ~
      pathPrefix(Segment){ packageName =>
        ensurePackagePresent(packageName) { singlePackageRelatedRoutes(packageName) }
      }
    }
  }

  private def singlePackageRelatedRoutes(packageName: String)(implicit request: HttpRequest): Route = {
    pathEnd { singlePackageRoute(packageName) } ~
    pathPrefix("classes"){
      pathEnd { allPackageClassesRoute(packageName) } ~
      pathPrefix(Segment){ className =>
        ensurePackageClassPresent(packageName, className) { singlePackageClassRoute(packageName, className) }
      }
    } ~
    pathPrefix("versions" / Segment) { versionName =>
      ensureArtifactPresent(packageName, versionName) { singleArtifactRelatedRoutes(packageName, versionName) }
    }
  }

  private def singleArtifactRelatedRoutes(packageName: String,
                                          versionName: String)(implicit request: HttpRequest): Route = {
    pathEnd{ singleArtifactRoute(packageName, versionName) } ~
    pathPrefix("jar") { singleArtifactJarDummyRoute(packageName, versionName) } ~
    pathPrefix("classes"){
      pathEnd { allArtifactClassesRoute(packageName, versionName) }~
      pathPrefix(Segment){ className =>
        ensureArtifactClassPresent(packageName, versionName, className) { singleArtifactClassRelatedRoutes(packageName, versionName, className) }
      }
    }
  }

  private def singleArtifactClassRelatedRoutes(packageName: String,
                                               versionName: String,
                                               className: String)(implicit request: HttpRequest): Route = {
    pathEnd { singleArtifactClassRoute(packageName, versionName, className) } ~
    pathPrefix("classfile") { singleArtifactClassFileDummyRoute(packageName, versionName, className) }
  }




  /*-----------------------------------------
   |           ROUTE IMPLEMENTATIONS        |
   -----------------------------------------*/

  private def allPackagesRoute(implicit request: HttpRequest): Route = get {
    extractPaginationHeaders(request, defaultLimit = 500){ (limit: Int, skip: Int) =>
      complete(requestHandler.getLibraries(skip, limit).toJson)
    }
  }

  private def singlePackageRoute(packageName: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getLibraryInfo(packageName).toJson)
  }
  private def allPackageClassesRoute(packageName: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getLibraryClassActivationInformation(packageName).toJson)
  }

  private def singlePackageClassRoute(packageName: String, className: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getLibraryClassInformation(packageName, className).toJson)
  }

  private def singleArtifactRoute(packageName: String, versionName: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getReleaseInfo(packageName, versionName).toJson)
  }
  private def singleArtifactJarDummyRoute(packageName: String, versionName: String)(implicit request: HttpRequest): Route = get {
    val suggestedDownloadFileName = s"$versionName-DUMMY.jar"

    completeWithBytes(requestHandler.getJar(packageName, versionName), suggestedDownloadFileName)
  }

  private def allArtifactClassesRoute(packageName: String, versionName: String)(implicit request: HttpRequest): Route = get {
    __TODO__completeNotImplemented
  }

  private def singleArtifactClassRoute(packageName:String, versionName: String, className: String): Route = get {
    complete(requestHandler.getClassInfo(packageName, versionName, className).toJson)
  }
  private def singleArtifactClassFileDummyRoute(packageName: String, versionName: String, className: String): Route = get {
    val suggestedDownloadFileName = className + ".class"

    completeWithBytes(requestHandler.getSingleClassFile(packageName, versionName, className), suggestedDownloadFileName)
  }


}
