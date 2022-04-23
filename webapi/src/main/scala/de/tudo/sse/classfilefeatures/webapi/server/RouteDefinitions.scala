package de.tudo.sse.classfilefeatures.webapi.server

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity.{ChunkStreamPart, Chunked}
import akka.http.scaladsl.model.StatusCodes.{Accepted, BadRequest, Conflict, InternalServerError, NotFound, NotImplemented}
import akka.http.scaladsl.model.headers.{ContentDispositionTypes, Location, `Content-Disposition`}
import akka.http.scaladsl.model.{ContentTypes, HttpRequest, HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import de.tudo.sse.classfilefeatures.webapi.core.RequestHandler
import de.tudo.sse.classfilefeatures.webapi.model.JsonSupport
import de.tudo.sse.classfilefeatures.webapi.model.requests.EnqueueRequest
import org.slf4j.{Logger, LoggerFactory}
import spray.json._

import scala.language.postfixOps
import scala.util.{Success, Try}



trait RouteDefinitions extends JsonSupport{

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  protected val requestHandler: RequestHandler
  protected implicit val theSystem: ActorSystem

  protected lazy val allApiRoutes: Route = pathPrefix("api") {
    extractRequest { implicit request =>
      processingRelatedRoutes ~
      packagesRelatedRoutes
    }
  }

  //  --------------------------------------------
  //  |      ROUTE DEFINITIONS                   |
  //  --------------------------------------------

  private def processingRelatedRoutes(implicit request:HttpRequest): Route = {
    pathPrefix("processing") {
      pathPrefix("enqueuePackage") { enqueuePackageRoute }
    }
  }

  private def packagesRelatedRoutes(implicit request: HttpRequest): Route = {
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
        ensurePackageClassPresent(packageName, className) { packageClassRelatedRoutes(packageName, className) }
      }
    } ~
    pathPrefix(Segment) { versionName =>
      ensureArtifactPresent(packageName, versionName) { singleArtifactRelatedRoutes(packageName, versionName) }
    }
  }

  private def packageClassRelatedRoutes(packageName: String, className: String)(implicit request: HttpRequest): Route = {
    __TODO__completeNotImplemented
  }

  private def singleArtifactRelatedRoutes(packageName: String,
                                          versionName: String)(implicit request: HttpRequest): Route = {
    pathEnd{ singleArtifactRoute(packageName, versionName) } ~
    pathPrefix("jar") { singleArtifactJarDummyRoute(packageName, versionName) } ~
    pathPrefix("classes"){
      pathEnd { allArtifactClassesRoute(packageName, versionName) }~
      pathPrefix(Segment){ className =>
        ensureArtifactClassPresent(packageName, versionName, className) { artifactClassRelatedRoutes(packageName, versionName, className) }
      }
    }
  }

  private def artifactClassRelatedRoutes(packageName: String,
                                         versionName: String,
                                         className: String)(implicit request: HttpRequest): Route = {
    pathEnd { singleArtifactClassRoute(packageName, versionName, className) } ~
    pathPrefix("classfile") { singleArtifactClassfileDummyRoute(packageName, versionName, className) }
  }



  //  --------------------------------------------
  //  |      ACTUAL ROUTE IMPLEMENTATIONS        |
  //  --------------------------------------------

  private def enqueuePackageRoute(implicit request: HttpRequest): Route = post {
    entityAs[EnqueueRequest]{ entity =>

      if(requestHandler.hasLibrary(entity.packageName)) {
        // Do not enqueue packages which are already fully processed => respond with 409 Conflict
        val packageUri = Uri(s"packages/${entity.packageName}")
        respondWithHeaders(Location(packageUri)) { complete(Conflict, s"Package ${entity.packageName} has already been processed.") }
      } else {
        val enqueueSuccess = requestHandler.processEnqueueLibraryRequest(entity.packageName)
        if(!enqueueSuccess) complete(InternalServerError)
        else complete(Accepted)
      }
    }
  }

  // Packages
  private def allPackagesRoute(implicit request: HttpRequest): Route = get {

    val skipTry = getHeaderValueIntOrElse("skip", 0)
    val limitTry = getHeaderValueIntOrElse("limit", 500)

    if(skipTry.isFailure || limitTry.isFailure){
      complete(BadRequest, "Header values for skip or limit are not valid integers")
    } else {
      complete(requestHandler.getLibraries(skipTry.get, limitTry.get).toJson)
    }

  }

  // Single Package
  private def singlePackageRoute(packageName: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getLibraryInfo(packageName).toJson)
  }

  private def allPackageClassesRoute(packageName: String)(implicit request: HttpRequest): Route = get {
    __TODO__completeNotImplemented
  }

  // Single Artifact
  private def singleArtifactRoute(packageName: String, versionName: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getReleaseInfo(packageName, versionName).toJson)
  }

  private def allArtifactClassesRoute(packageName: String, versionName: String)(implicit request: HttpRequest): Route = get {
    __TODO__completeNotImplemented
  }

  private def singleArtifactJarDummyRoute(packageName: String, versionName: String)(implicit request: HttpRequest): Route = get {
    val suggestedDownloadFileName = s"$versionName-DUMMY.jar"

    completeWithBytes(requestHandler.getJar(packageName, versionName), suggestedDownloadFileName)
  }

  // Single Class
  private def singleArtifactClassRoute(packageName: String, versionName: String, className: String)(implicit request: HttpRequest): Route = get {
    complete(requestHandler.getClassInfo(packageName, versionName, className).toJson)
  }

  private def singleArtifactClassfileDummyRoute(packageName: String, versionName: String, className: String)(implicit request: HttpRequest): Route = get {
    val suggestedDownloadFileName = className + ".class"

    completeWithBytes(requestHandler.getSingleClassFile(packageName, versionName, className), suggestedDownloadFileName)
  }




  //  --------------------------------------------
  //  |      PRIVATE HELPER METHODS              |
  //  --------------------------------------------

  private def ensurePackagePresent(packageName: String)(implicit route: Route):  Route = {
    if(requestHandler.hasLibrary(packageName)) {
      route
    } else if(requestHandler.configuration.enqueuePackageIdentifiersOnNotFound){

      log.info(s"Enqueueing package identifier '$packageName' following a 404 response.")

      val enqueueSuccess = requestHandler.processEnqueueLibraryRequest(packageName)
      if(enqueueSuccess) complete(Accepted, s"Package '$packageName' was not found in database, but has been scheduled for processing.")
      else complete(NotFound, s"Package '$packageName' was not found in database, appending to queue failed.")
    } else {
      complete(NotFound, s"Package '$packageName' was not found in database")
    }

  }

  private def ensureArtifactPresent(packageName: String, version: String)(implicit route: Route): Route = {
    if(requestHandler.hasRelease(packageName, version)) route
    else complete(NotFound, s"Version '$version' not found for package '$packageName'")
  }

  private def ensureArtifactClassPresent(packageName: String, version: String, className: String)(implicit route: Route): Route = {
    if(requestHandler.hasReleaseClass(packageName, version, className)) route
    else complete(NotFound, s"Class '$className' not found for version '$version' of package '$packageName'")
  }

  private def ensurePackageClassPresent(packageName: String, className: String)(implicit route: Route): Route = {
    if(requestHandler.hasLibraryClass(packageName, className)) route
    else complete(NotFound, s"Class $className not found for package $packageName")
  }

  private def completeWithBytes(byteSource: Source[Byte, NotUsed], fileName: String): Route = {
    val chunkedSource = byteSource
      .grouped(500)
      .map(chunkBytes => ChunkStreamPart(chunkBytes.toArray))

    val response = HttpResponse(entity = Chunked(ContentTypes.`application/octet-stream`, chunkedSource))
      .withHeaders(`Content-Disposition`(ContentDispositionTypes.attachment, Map("filename" -> fileName)))

    complete(response)
  }

  private def getHeaderValueRaw(headerName: String)(implicit request: HttpRequest): Option[String] =
    request.headers.find(h => h.name().equalsIgnoreCase(headerName)).map(_.value())

  private def getHeaderValueIntOrElse(headerName:String, defaultValue: Int)(implicit request: HttpRequest): Try[Int] = {
    getHeaderValueRaw(headerName).map(v => Try(v.toInt)).getOrElse(Success(defaultValue))
  }

  private def entityAs[T](route: T => Route)(implicit r: JsonReader[T]): Route = {

    entity(as[JsObject]) {e =>
      route(e.convertTo[T])
    }
  }

  private def __TODO__completeNotImplemented(implicit request: HttpRequest): Route = {
    log.warn(s"Route not implemented for ${request.uri.toString()}")
    complete(NotImplemented)
  }





}
