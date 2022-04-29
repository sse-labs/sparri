package de.tudo.sse.classfilefeatures.webapi.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import de.tudo.sse.classfilefeatures.webapi.core.RequestHandler
import de.tudo.sse.classfilefeatures.webapi.server.routes.ApiRouteDefinitions

import scala.concurrent.Future

class ApiServer(override val requestHandler: RequestHandler)
               (implicit val theSystem: ActorSystem) extends ApiRouteDefinitions {

  private val http = Http()

  def startServer(host: String, port: Integer): Future[ServerBinding] = {
    http
      .newServerAt(host, port)
      .bind(allApiRoutes)
  }

  def shutdown(): Future[Unit] = http.shutdownAllConnectionPools()

}
