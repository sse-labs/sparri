package de.tudo.sse.spareuse.eval.performance.dependencies
import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import de.tudo.sse.spareuse.eval.performance.gavToEntityId
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClients
import org.apache.http.protocol.HTTP
import org.apache.http.util.EntityUtils
import spray.json._

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ReuseBasedTransitiveDependencyAnalysis(baseUrl: String) extends TransitiveDependencyAnalysis {

  private val httpClient = HttpClients.createDefault()

  override def getAllDependencies(rootArtifactGAV: String): Try[Set[String]] = Try {
    val gavs = mutable.HashSet[String]()

    def addDepsRecursive(rootGav: String): Unit = {
      val newDirectDeps = getDirectDependencies(rootGav).diff(gavs)

      newDirectDeps.foreach(gavs.add)
      newDirectDeps.foreach(addDepsRecursive)
    }

    addDepsRecursive(rootArtifactGAV)


    gavs.toSet

  }



  def getDirectDependencies(artifactGav: String): Set[String] = {
    val entityIdent = gavToEntityId(artifactGav)

    val getRequest = new HttpGet(baseUrl + "/entities/" + entityIdent + "/results?analysis=mvn-dependencies:1.0.0")
    val response = httpClient.execute(getRequest)

    if(response.getStatusLine.getStatusCode != 200)
      throw new IllegalStateException("Failed to retrieve results with status code " + response.getStatusLine.getStatusCode)

    val bodyTry = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8))

    bodyTry.flatMap(b => Try(b.parseJson)) match {
      case Success(JsArray(values)) if values.nonEmpty && values.head.isInstanceOf[JsObject] =>
        val theResult = values.head.asInstanceOf[JsObject]

        if(theResult.fields.contains("jsonContent") && theResult.fields("jsonContent").isInstanceOf[JsString]){
          val resultJson = theResult.fields("jsonContent").asInstanceOf[JsString].value.parseJson.asInstanceOf[JsArray]

          val dependencyGAVs = resultJson.elements.map{
            case obj: JsObject =>
              val g = obj.fields("identifier").asJsObject.fields("groupId").asInstanceOf[JsString].value
              val a = obj.fields("identifier").asJsObject.fields("artifactId").asInstanceOf[JsString].value
              val v = obj.fields("identifier").asJsObject.fields("version").asInstanceOf[JsString].value
              g + ":" + a + ":" + v
            case _ =>
              throw new IllegalStateException("Malformed results")
          }

          dependencyGAVs.toSet
        } else {
          throw new IllegalStateException(s"Malformed results")
        }

      case _ =>
        throw new IllegalStateException(s"Invalid response for result $artifactGav")
    }


  }


  def ensureAllPartialResultsPresent(expectedGAVs: Set[String]): Unit = {

    val expectedIds = expectedGAVs.map(gavToEntityId)

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
