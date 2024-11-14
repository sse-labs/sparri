package org.anon.spareuse.webapi.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import org.anon.spareuse.webapi.core.{OracleResolutionRequestHandler, RequestHandler}
import org.anon.spareuse.webapi.core.RequestHandler
import org.anon.spareuse.webapi.server.routes.ApiRouteDefinitions

import scala.concurrent.Future

class ApiServer(override val requestHandler: RequestHandler, override val oracleRequestHandler: OracleResolutionRequestHandler)
               (implicit val theSystem: ActorSystem) extends ApiRouteDefinitions {

  private val http = Http()

  def startServer(host: String, port: Integer): Future[ServerBinding] = {
    http
      .newServerAt(host, port)
      .bind(allApiRoutes)
  }

  def shutdown(): Future[Unit] = http.shutdownAllConnectionPools()

}
