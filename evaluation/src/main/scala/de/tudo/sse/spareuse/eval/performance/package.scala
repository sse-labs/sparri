package de.tudo.sse.spareuse.eval

import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.protocol.HTTP
import org.apache.http.util.EntityUtils
import spray.json.{JsArray, JsObject, JsString, JsValue, enrichString}

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.util.{Success, Try}

package object performance {

  class TimedResult[T](content: T, durationMs: Long) {
    def getDurationMillis: Long = durationMs
    def getContent: T = content
  }

  def timedExec[T](op: () => T): TimedResult[T] = {
    val start = System.currentTimeMillis()

    val result = op()

    val duration = System.currentTimeMillis() - start

    new TimedResult(result, duration)
  }

  def gavToEntityId(gav: String): String = {
    if(gav.split(":").length != 3) throw new IllegalArgumentException("GAV must be separated by colons")
    val parts = gav.split(":")
    parts(0) + ":" + parts(1) + "!" + gav
  }


  def triggerEntityMining(entityId: String, baseUrl: String, httpClient: CloseableHttpClient): Option[String] = {
    val body = "{ \"Identifier\": \"" + entityId + "\"}"
    val url = baseUrl + "processing/enqueueEntity"

    val request = new HttpPost(url)
    request.setEntity(new StringEntity(body, "application/json", HTTP.DEFAULT_CONTENT_CHARSET))

    val response: CloseableHttpResponse = httpClient.execute(request)

    response.getStatusLine.getStatusCode match {
      case 302 => //Found
        val locationOpt = response
          .getAllHeaders
          .find(h => h.getName.equalsIgnoreCase("Location"))
          .map(h => h.getValue)
        EntityUtils.consume(response.getEntity)
        response.close()

        Some(locationOpt
          .map(rel => baseUrl + rel)
          .getOrElse(throw new IllegalStateException("Expected a location header to be returned")))
      case 202 => //Accepted
        EntityUtils.consume(response.getEntity)
        response.close()
        None
      case code@_ =>
        EntityUtils.consume(response.getEntity)
        response.close()
        throw new IllegalStateException("Invalid response from server: " + code)
    }
  }

  def triggerAnalysisRun(inputEntityIds: Set[String],
                         analysisName: String,
                         analysisVersion: String,
                         baseUrl: String,
                         httpClient: CloseableHttpClient,
                         configuration: String = "",
                         user: String = "test-runner"): Try[String]= {

    val uri = baseUrl + s"analyses/$analysisName/$analysisVersion/runs"
    val execRequest: HttpPost = new HttpPost(uri)

    // Build request body json
    val bodyBuilder /* :-) */ = new mutable.StringBuilder("{ \"Inputs\": [")
    bodyBuilder.append(inputEntityIds.map(id => "\"" + id + "\"").mkString(","))
    bodyBuilder.append("], \"Configuration\" :  \"")
    bodyBuilder.append(configuration)
    bodyBuilder.append("\", \"User\" : \"")
    bodyBuilder.append(user)
    bodyBuilder.append("\"}")
    val requestBody = bodyBuilder.toString()
    execRequest.setEntity(new StringEntity(requestBody, "application/json", HTTP.DEFAULT_CONTENT_CHARSET))

    var execResponse: CloseableHttpResponse = null

    Try {
      execResponse = httpClient.execute(execRequest)
      val statusCode = execResponse.getStatusLine.getStatusCode

      if (statusCode != 202 && statusCode != 302) { // Accepted or Found are fine
        val bodyTry = Try(EntityUtils.toString(execResponse.getEntity, StandardCharsets.UTF_8))
        throw HttpDownloadException(statusCode, uri, s"Non-Success status while attempting trigger analysis: ${bodyTry.getOrElse("No info")}")
      }

      val locationOpt = execResponse
        .getAllHeaders
        .find(h => h.getName.equalsIgnoreCase("Location"))
        .map(h => h.getValue)

      locationOpt.getOrElse(throw new IllegalStateException("Expected a location header to be returned"))
    }
  }

  def runFinished(runLocation: String, baseUrl: String, httpClient: CloseableHttpClient): Try[Boolean] = {

    val statusRequest = new HttpGet(baseUrl + runLocation)

    Try {
      val response = httpClient.execute(statusRequest)

      if (response.getStatusLine.getStatusCode != 200)
        throw new IllegalStateException("Failed to query status of analysis run, got response code " + response.getStatusLine.getStatusCode)

      EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson match {
        case obj: JsObject if obj.fields.contains("state") && obj.fields("state").isInstanceOf[JsString] =>
          val stateDescr = obj.fields("state").asInstanceOf[JsString].value
          !stateDescr.equalsIgnoreCase("Created") && !stateDescr.equalsIgnoreCase("Running")
        case other@_ =>
          throw new IllegalStateException(s"Unexpected JSON body type: $other")
      }

    }
  }

  def getAnalysisResultsForEntity(entityId: String, analysisName: String, analysisVersion: String,
                                  baseUrl: String, httpClient:CloseableHttpClient): Try[JsValue] = Try {

    val getRequest = new HttpGet(baseUrl + "/entities/" + entityId + "/results?analysis=" + analysisName + ":" + analysisVersion)
    val response = httpClient.execute(getRequest)

    if (response.getStatusLine.getStatusCode != 200)
      throw new IllegalStateException("Failed to retrieve results with status code " + response.getStatusLine.getStatusCode)

    val bodyTry = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8))

    bodyTry.flatMap(b => Try(b.parseJson)) match {
      case Success(JsArray(values)) if values.nonEmpty && values.head.isInstanceOf[JsObject] =>
        val theResult = values.head.asInstanceOf[JsObject]

        if (theResult.fields.contains("jsonContent") && theResult.fields("jsonContent").isInstanceOf[JsString]) {
          theResult.fields("jsonContent").asInstanceOf[JsString].value.parseJson
        } else {
          throw new IllegalStateException(s"Content of analysis result missing")
        }

      case _ =>
        throw new IllegalStateException(s"Invalid response for result $entityId")
    }
  }

}
