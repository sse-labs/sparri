package org.anon.spareuse.client.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding.{Get, Post}
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest, HttpResponse, Uri}
import org.anon.spareuse.client.ConfigReader
import org.anon.spareuse.core.model.RunState
import org.anon.spareuse.webapi.model.{AnalysisResultRepr, AnalysisRunRepr, JsonSupport}
import org.slf4j.{Logger, LoggerFactory}
import akka.util.ByteString

import scala.util.{Failure, Success, Try}
import spray.json.enrichString

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt

class SparriApiClient extends AutoCloseable with JsonSupport {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  protected[http] implicit val system: ActorSystem = ActorSystem("sparri-client")
  protected[http] implicit val ec: ExecutionContext = system.dispatcher


  def getAnalysisResultFor(analysisName: String, analysisVersion: String, input: String): Option[AnalysisResultRepr] = {

    getAsString(s"/api/entities/$input/results",
      queryParams = Map("analysis" -> s"$analysisName:$analysisVersion"),
      rawHeader = Map("limit" -> "10")) match {
      case Success(resultsJson) =>
        val results = resultsJson.parseJson.convertTo[List[AnalysisResultRepr]]

        if(results.size > 1)
          log.warn(s"Got multiple results of analysis $analysisName:$analysisVersion for entity $input")

        results.headOption
      case Failure(nfx: NotFoundException) =>
        log.warn(s"The requested entity $input is not known to the SPARRI server.", nfx)
        None
      case Failure(ex) =>
        log.error(s"Failed to request analysis", ex)
        None
    }
  }

  def analysisExecutedWith(analysisName: String, analysisVersion: String, input: String): Boolean = {
    getAsString(s"/api/entities/$input/processedBy",
      queryParams = Map("analysis" -> s"$analysisName:$analysisVersion"),
      rawHeader = Map("limit" -> "20")) match {
      case Success(runsJson) =>
        val runs = runsJson.parseJson.convertTo[List[AnalysisRunRepr]]

        runs.exists(run => run.State == RunState.Finished.toString)
      case Failure(nfx: NotFoundException) =>
        log.warn(s"The entity $input or the analysis $analysisName was not known to the SPARRI server.", nfx)
        false
      case Failure(ex) =>
        log.error(s"Unexpected error when querying analysis runs", ex)
        false
    }
  }

  private[http] def executeWithHeaders(request: HttpRequest, rawHeaders: Map[String, String] = Map.empty): Try[HttpResponse] = Try {
    val headers = rawHeaders.map{ case (name, value) => RawHeader(name, value)}.toSeq

    val response = Await.result(Http().singleRequest(request.withHeaders(headers)), 20.seconds)

    if(response.status.intValue() == 404)
      throw NotFoundException(s"Got 404: ${request.getUri()}")

    response
  }

  private[http] def buildUri(relPath: String, queryParams: Map[String, String] = Map.empty): Uri = {

    val queryStringOpt = if(queryParams.isEmpty) None else {
      Some(Query(queryParams).value)
    }

    Uri.from(scheme = "http", host = ConfigReader.getSparriHost, port = ConfigReader.getSparriPort, path = relPath, queryString = queryStringOpt)
  }


  protected[http] def postJsonRaw(relPath: String, jsonBody: Option[String], rawHeaders: Map[String, String] = Map.empty): Try[HttpResponse] = {
    val headers = rawHeaders.map{ case (key, value) => RawHeader(key, value)}.toSeq
    var request = Post(buildUri(relPath)).withHeaders(headers)

    if(jsonBody.isDefined){
      request = request.withEntity(HttpEntity(ContentTypes.`application/json`, jsonBody.get))
    }

    executeWithHeaders(request, rawHeaders)
  }

  protected[http] def postJsonAndReturnString(relPath: String, jsonBody: String, rawHeader: Map[String, String] = Map.empty): Try[String] = {
    postJsonRaw(relPath, Some(jsonBody), rawHeader)
      .map{ response =>
        val code = response.status.intValue()

        getStringEntity(response) match {
          case Success(stringEntity) =>
            if (code / 100 != 2)
              throw HttpResponseException(code, stringEntity)

            stringEntity
          case Failure(ex) =>
            log.error(s"Failed to read string entity", ex)
            throw ex
        }
      }
  }

  private[http] def getRaw(relPath: String, queryParams: Map[String, String] = Map.empty, rawHeaders: Map[String, String] = Map.empty): Try[HttpResponse] = {
    val get = Get(buildUri(relPath, queryParams))

    executeWithHeaders(get, rawHeaders)
  }

  def getAsString(relPath: String, queryParams: Map[String, String] = Map.empty, rawHeader: Map[String, String] = Map.empty): Try[String] = {
    getRaw(relPath, queryParams, rawHeader)
      .map{ response =>
        val code = response.status.intValue()

        getStringEntity(response) match {
          case Success(stringEntity) =>
            if (code / 100 != 2)
              throw HttpResponseException(code, stringEntity)

            stringEntity
          case Failure(ex) =>
            log.error(s"Failed to read string entity", ex)
            throw ex
        }
      }
  }

  private[http] def getStringEntity(response: HttpResponse): Try[String] = Try {
    Await.result(response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String), 20.seconds)
  }

  override def close(): Unit = {
    Http().shutdownAllConnectionPools()
    system.terminate()
  }

  private case class NotFoundException(msg: String) extends Throwable {
    override def getMessage: String = msg
  }

  case class HttpResponseException(code: Int, msg: String) extends Throwable {
    override def getMessage: String = s"Got response code $code: $msg"
  }

}
