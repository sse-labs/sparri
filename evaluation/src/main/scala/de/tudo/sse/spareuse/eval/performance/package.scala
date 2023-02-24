package de.tudo.sse.spareuse.eval

import org.apache.http.client.HttpClient
import org.apache.http.client.methods.{CloseableHttpResponse, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.protocol.HTTP
import org.apache.http.util.EntityUtils

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

}
