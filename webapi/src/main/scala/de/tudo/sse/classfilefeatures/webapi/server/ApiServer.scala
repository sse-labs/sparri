package de.tudo.sse.classfilefeatures.webapi.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes.NotImplemented
import akka.http.scaladsl.server.Directives.{complete, path, pathPrefix}
import akka.http.scaladsl.server.{PathMatcher1, Route}
import de.tudo.sse.classfilefeatures.webapi.core.RequestHandler

import scala.concurrent.Future
import scala.language.postfixOps

class ApiServer(requestHandler: RequestHandler)(private implicit val theSystem: ActorSystem) {

  private val http = Http()

  private val dummyClassfileMatcher: PathMatcher1[String] = Segment / "dummy-classfiles"

  private val routes: Route = pathPrefix("api") {

      pathPrefix("libraries") {
        pathEnd {
          //TODO: Serialization, Pagination
          complete("[" + requestHandler.getLibraries().mkString(", ") + "]")
        } ~ pathPrefix(Segment){ libraryName =>
          ensureLibraryPresent(libraryName) {

            pathEnd {
              complete("")
            } ~ path(dummyClassfileMatcher) { version =>
              ensureArtifactPresent(libraryName, version) {
                complete(NotImplemented)
              }
            }
          }
        }
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


}
