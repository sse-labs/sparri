package org.anon.spareuse.client.http

import org.anon.spareuse.client.ConfigReader
import org.anon.spareuse.webapi.model.{AnalysisInformationRepr, AnalysisResultRepr, AnalysisRunRepr, JsonSupport}
import org.apache.http.client.HttpResponseException
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.DefaultHttpRequestFactory
import org.apache.http.{Header, HttpHost, HttpRequest}
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicHeader
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}
import spray.json.enrichString

import java.net.URI

class SparriApiClient extends AutoCloseable with JsonSupport {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private[http] val httpClient = HttpClients.createDefault()
  private[http] val sparriHost = new HttpHost(ConfigReader.getSparriHost, ConfigReader.getSparriPort)


  def getAnalysisResultFor(analysisName: String, analysisVersion: String, input: String): Option[AnalysisResultRepr] = {

    getAsString(s"/api/entities/$input/results",
      queryParams = Map("analysis" -> s"$analysisName:$analysisVersion"),
      rawHeader = Map("limit" -> "10")) match {
      case Success(resultsJson) =>
        val results = resultsJson.parseJson.convertTo[List[AnalysisResultRepr]]

        if(results.size > 1)
          log.warn(s"Got multiple results of analysis $analysisName:$analysisVersion for entity $input")

        results.headOption
      case Failure(hx: HttpResponseException) if hx.getStatusCode == 404 =>
        log.warn(s"The requested entity $input is not known to the SPARRI server.", hx)
        None
      case Failure(ex) =>
        log.error(s"Failed to request analysis", ex)
        None
    }
  }

  def analysisExecutedWith(analysisName: String, analysisVersion: String, input: String): Boolean = {
    getAsString(s"/api/analyses/$analysisName/$analysisVersion/runs",
      queryParams = Map("input" -> input), rawHeader = Map("limit" -> "10")) match {
      case Success(runsJson) =>
        val runs = runsJson.parseJson.convertTo[List[AnalysisRunRepr]]

        runs.exists(run => run.State == "Finished")

      case Failure(hx: HttpResponseException) if hx.getStatusCode == 404 =>
        log.warn(s"The entity $input or the analysis $analysisName was not known to the SPARRI server.", hx)
        false
      case Failure(ex) =>
        log.error(s"Unexpected error when querying analysis runs", ex)
        false
    }
  }




  private[http] def executeWithHeaders(request: HttpRequest, rawHeaders: Map[String, String] = Map.empty): Try[CloseableHttpResponse] = Try {
    val headers = rawHeaders.map { case (name, value) => new BasicHeader(name, value) }.toArray[Header]
    request.setHeaders(headers)
    httpClient.execute(sparriHost, request)
  }

  private[http] def buildUri(relPath: String, queryParams: Map[String, String] = Map.empty): URI = {
    val builder = new URIBuilder()
      .setHost(ConfigReader.getSparriHost)
      .setPort(ConfigReader.getSparriPort)
      .setScheme("http")
      .setPath(relPath)

    queryParams.foreach{ case (key, value) => builder.setParameter(key, value)}

    builder.build()
  }

  private[http] def getRaw(relPath: String, queryParams: Map[String, String] = Map.empty, rawHeaders: Map[String, String] = Map.empty): Try[CloseableHttpResponse] = {
    val get = new HttpGet(buildUri(relPath, queryParams))

    executeWithHeaders(get, rawHeaders)
  }

  def getAsString(relPath: String, queryParams: Map[String, String] = Map.empty, rawHeader: Map[String, String] = Map.empty): Try[String] = {
    getRaw(relPath, queryParams, rawHeader)
      .map{ response =>
        val code = response.getStatusLine.getStatusCode
        val entity = response.getEntity

        val entityInputStream = entity.getContent
        var charSetOpt: Option[String] = None
        if(entity.getContentEncoding != null){
          charSetOpt = Some(entity.getContentEncoding.getValue)
        }

        val entityBytes = LazyList.continually(entityInputStream.read).takeWhile(_ != -1).map(_.toByte).toArray

        val stringEntity = if(charSetOpt.isDefined) new String(entityBytes, charSetOpt.get) else new String(entityBytes)

        entityInputStream.close()
        response.close()

        if(code / 100 != 2)
          throw new HttpResponseException(code, stringEntity)

        stringEntity
      }
  }

  override def close(): Unit = {
    val closeTry = Try(httpClient.close())
    if(closeTry.isFailure){
      log.warn("Failed to close HTTP client.", closeTry.failed.get)
    }
  }


}
