package de.tudo.sse.spareuse.eval.performance.dependencies
import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClients
import org.apache.http.protocol.HTTP
import org.apache.http.util.EntityUtils

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}

class ReuseBasedTransitiveDependencyAnalysis(baseUrl: String) extends TransitiveDependencyAnalysis {

  private val httpClient = HttpClients.createDefault()

  override def getAllDependencies(rootArtifactGAV: String): Try[Set[String]] = Success(Set.empty) //TODO!

  def ensureAllPartialResultsPresent(expectedGAVs: Set[String]): Unit = {

    val expectedIds = expectedGAVs.map { gav =>
      val parts = gav.split(":")
      s"${parts(0)}:${parts(1)}" + "!" + gav
    }

    val uri = baseUrl + "analyses/mvn-dependencies/1.0.0/runs"
    val execRequest: HttpPost = new HttpPost(uri)
    val requestBody = "{ \"Inputs\": [" + {expectedIds.map(gav => "\"" + gav + "\"").mkString(",")} + "], \"Configuration\" : \"\", \"User\" : \"test-runner\"}"
    execRequest.setEntity(new StringEntity(requestBody, "application/json", HTTP.DEFAULT_CONTENT_CHARSET))

    var execResponse: CloseableHttpResponse = null

    Try {
      execResponse = httpClient.execute(execRequest)
      val statusCode = execResponse.getStatusLine.getStatusCode

      if(statusCode != 202 && statusCode != 302) { // Accepted or Found are fine
        val bodyTry = Try(EntityUtils.toString(execResponse.getEntity, StandardCharsets.UTF_8))
        throw  HttpDownloadException(statusCode, uri, s"Non-Success status while attempting trigger analysis: ${bodyTry.getOrElse("No info")}")
      }

      val locationOpt = execResponse
        .getAllHeaders
        .find( h => h.getName.equalsIgnoreCase("Location"))
        .map(h => h.getValue)

      locationOpt.getOrElse(throw new IllegalStateException("Expected a location header to be returned"))
    } match {
      case Success(location) =>

        logger.info(s"Successfully triggered analysis run. Waiting for run at $location to complete...")

        def isFinished = {
          import spray.json._

          val statusRequest = new HttpGet(baseUrl + location)

          Try {
            val response = httpClient.execute(statusRequest)

            if(response.getStatusLine.getStatusCode != 200)
              throw new IllegalStateException("Failed to query status of analysis run, got response code " + response.getStatusLine.getStatusCode)

            EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson match {
              case obj: JsObject if obj.fields.contains("state") && obj.fields("state").isInstanceOf[JsString] =>
                val stateDescr = obj.fields("state").asInstanceOf[JsString].value
                logger.debug("Run state is: " + stateDescr)
                !stateDescr.equalsIgnoreCase("Created") && !stateDescr.equalsIgnoreCase("Running")
              case other@_ =>
                logger.error(s"Unexpected JSON body: $other")
                throw new IllegalStateException("Unexpected JSON body type")
            }

          }
        }


        while(!isFinished.get){
          Thread.sleep(500)
        }

        logger.info("Result available.")
      case Failure(ex) =>
        logger.error("Failed to trigger analysis", ex)
    }


  }

  def close(): Unit = httpClient.close()

}
