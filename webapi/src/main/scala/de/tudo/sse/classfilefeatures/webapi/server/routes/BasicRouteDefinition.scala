package de.tudo.sse.classfilefeatures.webapi.server.routes

import akka.NotUsed
import akka.http.scaladsl.model.HttpEntity.{ChunkStreamPart, Chunked}
import akka.http.scaladsl.model.{ContentTypes, HttpRequest, HttpResponse}
import akka.http.scaladsl.model.StatusCodes.{Accepted, BadRequest, NotFound, NotImplemented}
import akka.http.scaladsl.model.headers.{ContentDispositionTypes, `Content-Disposition`}
import akka.http.scaladsl.server.Directives.{as, complete, entity}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import de.tudo.sse.classfilefeatures.webapi.core.RequestHandler
import de.tudo.sse.classfilefeatures.webapi.model.JsonSupport
import org.slf4j.{Logger, LoggerFactory}
import spray.json.{JsObject, JsonReader}

import scala.util.{Success, Try}

trait BasicRouteDefinition extends JsonSupport {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  protected val requestHandler: RequestHandler


  //  ------------------------------
  //  |    404 UTILITIES           |
  //  ------------------------------
  protected def ensurePackagePresent(packageName: String)(implicit route: Route):  Route = {
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

  protected def ensureArtifactPresent(packageName: String, version: String)(implicit route: Route): Route = {
    if(requestHandler.hasRelease(packageName, version)) route
    else complete(NotFound, s"Version '$version' not found for package '$packageName'")
  }

  protected def ensureArtifactClassPresent(packageName: String, version: String, className: String)(implicit route: Route): Route = {
    if(requestHandler.hasReleaseClass(packageName, version, className)) route
    else complete(NotFound, s"Class '$className' not found for version '$version' of package '$packageName'")
  }

  protected def ensurePackageClassPresent(packageName: String, className: String)(implicit route: Route): Route = {
    if(requestHandler.hasLibraryClass(packageName, className)) route
    else complete(NotFound, s"Class $className not found for package $packageName")
  }

  protected def ensureAnalysisPresent(analysisName: String)(implicit route: Route): Route = {
    if(true) route //TODO: Actual implementation
    else complete(NotFound, s"Analysis $analysisName was not found in database")
  }


  //  ------------------------------
  //  |    HTTP UTILITIES          |
  //  ------------------------------

  protected def completeWithBytes(byteSource: Source[Byte, NotUsed], fileName: String): Route = {
    val chunkedSource = byteSource
      .grouped(500)
      .map(chunkBytes => ChunkStreamPart(chunkBytes.toArray))

    val response = HttpResponse(entity = Chunked(ContentTypes.`application/octet-stream`, chunkedSource))
      .withHeaders(`Content-Disposition`(ContentDispositionTypes.attachment, Map("filename" -> fileName)))

    complete(response)
  }

  protected def extractPaginationHeaders(request: HttpRequest,
                                         defaultLimit: Int = 100,
                                         defaultSkip: Int = 0)(implicit route: (Int, Int) => Route): Route = {
    val skipTry = getHeaderValueIntOrElse("skip", defaultSkip)(request)
    val limitTry = getHeaderValueIntOrElse("limit", defaultLimit)(request)

    if(skipTry.isFailure || limitTry.isFailure){
      complete(BadRequest, "Header values for skip or limit are not valid integers")
    } else {
      route(limitTry.get, skipTry.get)
    }
  }

  protected def getHeaderValueRaw(headerName: String)(implicit request: HttpRequest): Option[String] =
    request.headers.find(h => h.name().equalsIgnoreCase(headerName)).map(_.value())

  protected def getHeaderValueIntOrElse(headerName:String, defaultValue: Int)(implicit request: HttpRequest): Try[Int] = {
    getHeaderValueRaw(headerName).map(v => Try(v.toInt)).getOrElse(Success(defaultValue))
  }

  protected def entityAs[T](route: T => Route)(implicit r: JsonReader[T]): Route = {

    entity(as[JsObject]) {e =>
      route(e.convertTo[T])
    }
  }

  protected def __TODO__completeNotImplemented(implicit request: HttpRequest): Route = {
    log.warn(s"Route not implemented for ${request.uri.toString()}")
    complete(NotImplemented)
  }



}
