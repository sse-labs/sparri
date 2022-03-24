package de.tudo.sse.classfilefeatures.webapi.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.StatusCodes.NotImplemented
import akka.http.scaladsl.server.Directives.{complete, path, pathPrefix}
import akka.http.scaladsl.server.{PathMatcher1, Route}

import scala.concurrent.Future

class ApiServer(private implicit val theSystem: ActorSystem) {

  private val http = Http()


  private val libraryMatcher: PathMatcher1[String] = "libraries" / Segment


  private val dummyClassfileMatcher: PathMatcher1[String] = Segment / "dummy-classfiles"

  private val routes: Route = pathPrefix("api") {
      pathPrefix(libraryMatcher) { libraryName =>
        //TODO: Check library present

        path(dummyClassfileMatcher) { version =>
          complete(NotImplemented)
        }
      }
  }




  def startServer(host: String, port: Integer): Future[ServerBinding] = {
    http.newServerAt(host, port).bind(routes)
  }

  def shutdown(): Future[Unit] = http.shutdownAllConnectionPools()


}
