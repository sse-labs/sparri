package org.anon.spareuse

import com.typesafe.config.ConfigFactory
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod, JavaPackage}
import org.anon.spareuse.core.utils.fromHex
import org.anon.spareuse.core.utils.http.HttpDownloadException
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.protocol.HTTP
import org.apache.http.util.EntityUtils
import org.neo4j.driver.internal.shaded.io.netty.handler.codec.json.JsonObjectDecoder
import spray.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, enrichString}

import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

package object eval {

  def getApiBaseUrl: String = {
    val config = ConfigFactory.load()
    if (config.hasPath("spar-reuse.evaluation.api"))
      config.getString("spar-reuse.evaluation.api")
    else "http://localhost:9090/api"
  }

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
    if (gav.split(":").length != 3) throw new IllegalArgumentException("GAV must be separated by colons")
    val parts = gav.split(":")
    parts(0) + ":" + parts(1) + "!" + gav
  }


  def triggerEntityMining(entityId: String, baseUrl: String, httpClient: CloseableHttpClient): Option[String] = {
    val body = "{ \"Identifier\": \"" + entityId + "\"}"
    val url = baseUrl + "processing/enqueueEntity"

    val request = new HttpPost(url)
    request.setEntity(new StringEntity(body, "application/json"))

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
                         user: String = "test-runner",
                         baselineRun: Option[String] = None): Try[String] = {

    val uri = baseUrl + s"analyses/$analysisName/$analysisVersion/runs"
    val execRequest: HttpPost = new HttpPost(uri)

    // Build request body json
    val bodyBuilder /* :-) */ = new mutable.StringBuilder("{ \"Inputs\": [")
    bodyBuilder.append(inputEntityIds.map(id => "\"" + id + "\"").mkString(","))
    bodyBuilder.append("], \"Configuration\" :  \"")
    bodyBuilder.append(configuration)
    bodyBuilder.append("\", \"User\" : \"")
    bodyBuilder.append(user)

    if(baselineRun.isDefined){
      bodyBuilder.append("\", \"BaselineRun\" : \"")
      bodyBuilder.append(baselineRun.get)
    }

    bodyBuilder.append("\"}")
    val requestBody = bodyBuilder.toString()
    val entity = new  StringEntity(requestBody, StandardCharsets.UTF_8)
    entity.setContentType("application/json")
    execRequest.setEntity(entity)

    var execResponse: CloseableHttpResponse = null

    Try {
      execResponse = httpClient.execute(execRequest)
      val statusCode = execResponse.getStatusLine.getStatusCode

      if (statusCode != 200 && statusCode != 202 && statusCode != 302) { // Accepted or Found are fine
        val bodyTry = Try(EntityUtils.toString(execResponse.getEntity, StandardCharsets.UTF_8))
        execResponse.close()
        throw HttpDownloadException(statusCode, uri, s"Non-Success status while attempting trigger analysis: ${bodyTry.getOrElse("No info")}")
      }

      val locationOpt = execResponse
        .getAllHeaders
        .find(h => h.getName.equalsIgnoreCase("Location"))
        .map(h => h.getValue)

      execResponse.close()

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

  def getAllVersionsForLibrary(ga: String, baseUrl: String, httpClient: CloseableHttpClient): Try[Seq[String]] = Try {
    val releasesRequest = new HttpGet(baseUrl + "entities/" + ga + "/children")
    releasesRequest.setHeader("limit", "1000")
    val response = httpClient.execute(releasesRequest)

    if(response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve releases for library $ga (Status code ${response.getStatusLine.getStatusCode}) ")
    }
    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    contentT match {
      case Success(JsArray(values)) =>
        values.collect{
          case jo: JsObject =>
            val gav = jo.fields("Name").asInstanceOf[JsString].value
            gav.split(":")(2)
        }
      case Failure(ex) =>
        throw ex
      case _ =>
        throw new IllegalStateException(s"Invalid format returned by server")
    }

  }

  def getAllTypesForProgram(gav: String, baseUrl: String, httpClient: CloseableHttpClient): Try[Seq[JavaClass]] = Try {
    val packagesRequest = new HttpGet(baseUrl + "entities/" + gavToEntityId(gav) + "/children")
    packagesRequest.setHeader("limit", "1000")
    val response = httpClient.execute(packagesRequest)

    if (response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve packages for program $gav (Status code ${response.getStatusLine.getStatusCode}) ")
    }

    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    contentT match {
      case Success(JsArray(values)) =>
        values
          .collect {
            case jo: JsObject =>
              new JavaPackage(
                jo.fields("Name").asInstanceOf[JsString].value,
                jo.fields("ID").asInstanceOf[JsNumber].value.toLongExact,
                jo.fields("Repository").asInstanceOf[JsString].value)
          }.flatMap { p =>
          val allTypes = getAllTypesForPackage(p.uid, baseUrl, httpClient).get
          allTypes.foreach(_.setParent(p))
          allTypes
        }
      case Failure(ex) =>
        throw new IllegalStateException(s"Failed to retrieve packages for program $gav", ex)
      case _ =>
        throw new IllegalStateException("Invalid response formats")
    }

  }

  def encodeString(s: String): String = s.replace("/", "%2F").replace("!", "%21").replace(":", "%3A")

  def getAllTypesForPackage(packageIdent: String, baseUrl: String, httpClient: CloseableHttpClient): Try[Seq[JavaClass]] = Try {

    val encodedPackageIdent = encodeString(packageIdent)

    val typesRequest = new HttpGet(baseUrl + "entities/" + encodedPackageIdent + "/children")
    typesRequest.setHeader("limit", "1000")
    val response = httpClient.execute(typesRequest)

    if (response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve types for package $packageIdent (Status code ${response.getStatusLine.getStatusCode}) ")
    }

    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    contentT match {
      case Success(JsArray(values)) =>
        values.collect {
          case jo: JsObject =>
            new JavaClass(
              jo.fields("Name").asInstanceOf[JsString].value,
              jo.fields("ThisTypeFqn").asInstanceOf[JsString].value,
              jo.fields("ID").asInstanceOf[JsNumber].value.toLongExact,
              jo.fields.get("SuperTypeFqn").map {
                case s: JsString => s.value
                case _ => throw new IllegalStateException("Invalid response format")
              },
              jo.fields("InterfaceTypeFqns") match {
                case a: JsArray =>
                  a.elements.map {
                    case s: JsString => s.value
                    case _ => throw new IllegalStateException("Invalid response format")
                  }.toSet
                case _ => throw new IllegalStateException("Invalid response format")
              },
              jo.fields("IsInterface").asInstanceOf[JsBoolean].value,
              jo.fields("IsFinal").asInstanceOf[JsBoolean].value,
              jo.fields("IsAbstract").asInstanceOf[JsBoolean].value,
              jo.fields("Repository").asInstanceOf[JsString].value,
              fromHex(jo.fields("Hash").asInstanceOf[JsString].value))
        }
      case Failure(ex) =>
        throw new IllegalStateException(s"Failed to retrieve types for package $packageIdent", ex)
      case _ =>
        throw new IllegalStateException("Invalid response formats")
    }
  }

  def getAllMethodsForClass(classUid: String, baseUrl: String, httpClient: CloseableHttpClient): Try[Seq[JavaMethod]] = Try {
    val getRequest = new HttpGet(baseUrl + "/entities/" + encodeString(classUid) + "/children")

    getRequest.setHeader("limit", "1000")
    val response = httpClient.execute(getRequest)

    if (response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve methods for class $classUid (Status code ${response.getStatusLine.getStatusCode}) ")
    }

    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    contentT match {
      case Success(JsArray(values)) =>
        values.collect {
          case jo: JsObject =>
            new JavaMethod(
              jo.fields("Name").asInstanceOf[JsString].value,
              jo.fields("Descriptor").asInstanceOf[JsString].value,
              jo.fields("ID").asInstanceOf[JsNumber].value.toLongExact,
              jo.fields("IsFinal").asInstanceOf[JsBoolean].value,
              jo.fields("IsStatic").asInstanceOf[JsBoolean].value,
              jo.fields("IsAbstract").asInstanceOf[JsBoolean].value,
              jo.fields("Visibility").asInstanceOf[JsString].value,
              jo.fields("Repository").asInstanceOf[JsString].value,
              jo.fields("MethodHash").asInstanceOf[JsNumber].value.intValue
            )
        }
      case Failure(ex) =>
        throw new IllegalStateException(s"Failed to retrieve methods for class $classUid", ex)
      case _ =>
        throw new IllegalStateException("Invalid response formats")
    }
  }

  def getAnalysisResultsForEntity(entityId: String, analysisName: String, analysisVersion: String,
                                  baseUrl: String, httpClient: CloseableHttpClient): Try[JsValue] = Try {

    val getRequest = new HttpGet(baseUrl + "/entities/" + entityId + "/results?analysis=" + analysisName + ":" + analysisVersion)
    val response = httpClient.execute(getRequest)

    if (response.getStatusLine.getStatusCode != 200)
      throw HttpDownloadException(response.getStatusLine.getStatusCode, getRequest.getURI.toString, s"Failed to retrieve results for entity $entityId")

    val bodyTry = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8))

    response.close()

    bodyTry.flatMap(b => Try(b.parseJson)) match {
      case Success(JsArray(values)) if values.nonEmpty && values.head.isInstanceOf[JsObject] =>
        val theResult = values.head.asInstanceOf[JsObject]

        if (theResult.fields.contains("jsonContent") && theResult.fields("jsonContent").isInstanceOf[JsString]) {
          theResult.fields("jsonContent").asInstanceOf[JsString].value.parseJson
        } else {
          throw new IllegalStateException(s"Content of analysis result missing")
        }

      case Success(JsArray(values)) if values.isEmpty =>
        throw HttpDownloadException(404, getRequest.getURI.toString, s"No results exist on entity ${entityId} for analysis $analysisName:$analysisVersion")

      case Failure(ex) =>
        throw new IllegalStateException(s"Invalid response for result $entityId", ex)
      case _ =>
        throw new IllegalStateException(s"Invalid response for result $entityId")
    }
  }

}
