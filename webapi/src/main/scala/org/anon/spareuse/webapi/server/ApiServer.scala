package org.anon.spareuse.webapi.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.settings.ServerSettings
import akka.http.scaladsl.settings.ServerSettings.timeoutsShortcut
import org.anon.spareuse.webapi.core.{OracleResolutionRequestHandler, RequestHandler}
import org.anon.spareuse.webapi.core.RequestHandler
import org.anon.spareuse.webapi.server.routes.ApiRouteDefinitions

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class ApiServer(override val requestHandler: RequestHandler, override val oracleRequestHandler: OracleResolutionRequestHandler)
               (implicit val theSystem: ActorSystem) extends ApiRouteDefinitions {

  private val http = Http()

  def startServer(host: String, port: Integer): Future[ServerBinding] = {
    http
      .newServerAt(host, port)
      .adaptSettings(s => s.withTimeouts(s.withRequestTimeout(60.seconds)))
      .bind(allApiRoutes)
  }

  def shutdown(): Future[Unit] = http.shutdownAllConnectionPools()

}
