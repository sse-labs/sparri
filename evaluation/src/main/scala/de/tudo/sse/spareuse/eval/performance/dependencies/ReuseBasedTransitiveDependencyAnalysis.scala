package de.tudo.sse.spareuse.eval.performance.dependencies
import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import org.apache.http.client.methods.{CloseableHttpResponse, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClients

import scala.util.{Success, Try}

class ReuseBasedTransitiveDependencyAnalysis(baseUrl: String) extends TransitiveDependencyAnalysis {

  private val httpClient = HttpClients.createDefault()

  override def getAllDependencies(rootArtifactGAV: String): Try[Set[String]] = Success(Set.empty) //TODO!

  def ensureAllPartialResultsPresent(expectedGAVs: Set[String]): Unit = {

    val uri = baseUrl + "analyses/mvn-dependencies/1.0.0/runs"
    val execRequest: HttpPost = new HttpPost(uri)
    val requestBody = "{Inputs: [" + {expectedGAVs.map(gav => "\"" + gav + "\"").mkString(",")} + "], Configuration: \"\", User:\"test-runner\"}"
    execRequest.setEntity(new StringEntity(requestBody))

    var execResponse: CloseableHttpResponse = null

    val runLocationTry = Try {
      execResponse = httpClient.execute(execRequest)
      val statusCode = execResponse.getStatusLine.getStatusCode

      if(statusCode != 202 && statusCode != 302) // Accepted or Found are fine
        throw  HttpDownloadException(statusCode, uri, s"Non-Success status while attempting trigger analysis.")

      val locationOpt = execResponse
        .getAllHeaders
        .find( h => h.getName.equalsIgnoreCase("Location"))
        .map(h => h.getValue)

      locationOpt.getOrElse(throw new IllegalStateException("Expected a location header to be returned"))
    }

    //TODO: Query status of that run until it finished, needs API endpoint first!
    ???


  }

  def close(): Unit = httpClient.close()

}
