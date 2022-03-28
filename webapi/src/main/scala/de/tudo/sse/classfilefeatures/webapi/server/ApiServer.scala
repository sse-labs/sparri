package de.tudo.sse.classfilefeatures.webapi.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.model.StatusCodes.{BadRequest, NotFound, NotImplemented}
import akka.http.scaladsl.server.Directives.{complete, pathPrefix}
import akka.http.scaladsl.server.Route
import de.tudo.sse.classfilefeatures.webapi.core.RequestHandler
import de.tudo.sse.classfilefeatures.webapi.model.JsonSupport
import spray.json.enrichAny

import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Success, Try}

class ApiServer(requestHandler: RequestHandler)(private implicit val theSystem: ActorSystem) extends JsonSupport{

  private val http = Http()

  private val routes: Route =
    pathPrefix("api") {

      extractRequest { implicit request =>

        pathPrefix("libraries") {
          pathEnd { allLibrariesRoute() } ~
          pathPrefix(Segment){ libraryName =>

            ensureLibraryPresent(libraryName) {

              pathEnd { singleLibraryRoute(libraryName) } ~
              pathPrefix("classes") {
                pathEnd { allLibraryClassfilesRoute(libraryName) } ~
                pathPrefix(Segment) { className => singleLibraryClassfileRoute(libraryName, className)}
              } ~
              pathPrefix(Segment){ releaseName =>
                ensureArtifactPresent(libraryName, releaseName) {
                  pathEnd { singleReleaseRoute(libraryName, releaseName) } ~
                  pathPrefix("classes") {
                    pathEnd { allReleaseClassfilesRoute(libraryName, releaseName) } ~
                      pathPrefix(Segment) { className => singleReleaseClassfileRoute(libraryName, releaseName, className)}
                  }
                }
              }
            }
          }
        }
      }
   }


  private def allLibrariesRoute()(implicit request: HttpRequest): Route = {
    val skipTry = getHeaderValueRaw("skip").map(v => Try(v.toInt)).getOrElse(Success(0))
    val limitTry = getHeaderValueRaw("limit").map(v => Try(v.toInt)).getOrElse(Success(500))

    if(skipTry.isFailure || limitTry.isFailure){
      complete(BadRequest, "Header values for skip or limit are not valid integers")
    } else {
      complete(requestHandler.getLibraries(skipTry.get, limitTry.get).toJson)
    }

  }

  private def singleLibraryRoute(libName: String)(implicit request: HttpRequest): Route = {
    complete(requestHandler.getLibraryInfo(libName).toJson)
  }

  private def singleReleaseRoute(libName: String, release: String)(implicit request: HttpRequest): Route = {
    complete(requestHandler.getReleaseInfo(libName, release).toJson)
  }

  private def allLibraryClassfilesRoute(libName: String)(implicit request: HttpRequest): Route = {
    complete(NotImplemented)
  }

  private def allReleaseClassfilesRoute(libName: String, releaseName: String)(implicit request: HttpRequest): Route = {
    complete(NotImplemented)
  }

  private def singleLibraryClassfileRoute(libName: String, className: String)(implicit request: HttpRequest): Route = {
    if(requestHandler.hasLibraryClass(libName, className)){
      complete(NotImplemented)
    } else {
      complete(NotFound, s"Class $className has not been found for library $libName")
    }
  }

  private def singleReleaseClassfileRoute(libName: String, releaseName: String, className: String)(implicit request: HttpRequest): Route = {
    if(requestHandler.hasReleaseClass(libName, releaseName, className)){
      complete(NotImplemented)
    } else {
      complete(NotFound, s"Class $className has not been found for release $releaseName of library $libName")
    }
  }




  def startServer(host: String, port: Integer): Future[ServerBinding] = {
    http.newServerAt(host, port).bind(routes)
  }

  def shutdown(): Future[Unit] = http.shutdownAllConnectionPools()

  private def ensureLibraryPresent(libraryName: String)(implicit route: Route):  Route = {
    if(requestHandler.hasLibrary(libraryName)) route
    else complete(StatusCodes.NotFound, s"Library '$libraryName' was not found in database")
  }

  private def ensureArtifactPresent(libraryName: String, version: String)(implicit route: Route): Route = {
    if(requestHandler.hasRelease(libraryName, version)) route
    else complete(StatusCodes.NotFound, s"Version '$version' not found for library '$libraryName'")
  }

  private def getHeaderValueRaw(headerName: String)(implicit request: HttpRequest): Option[String] =
    request.headers.find(h => h.name().equalsIgnoreCase(headerName)).map(_.value())


}
