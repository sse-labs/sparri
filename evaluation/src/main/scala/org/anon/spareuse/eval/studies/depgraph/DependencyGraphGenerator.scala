package org.anon.spareuse.eval.studies.depgraph

import org.anon.spareuse.eval.{gavToEntityId, getAnalysisResultsForEntity, getApiBaseUrl}
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.util.EntityUtils
import org.neo4j.driver.{AuthTokens, Config, Driver, GraphDatabase, Logging}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.charset.StandardCharsets
import spray.json.{JsArray, JsObject, JsString, enrichString}

import java.util
import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._

class DependencyGraphGenerator(dbUri: String, dbUser: String, dbPass: String) {

  private val cypherArtifactBatchInsert = "UNWIND $gavs AS gav MERGE (a: Artifact { id: gav })"
  private val cypherEdgeBatchInsert = "UNWIND $deps AS dep MATCH (a1:Artifact {id: dep.source}) MERGE (a2: Artifact {id: dep.target}) CREATE (a1)-[:dependsOn]->(a2)"

  private val apiBaseUrl = getApiBaseUrl
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val batchSize = 1000

  private lazy val driverConfig: Config = Config.builder().withLogging(Logging.none())
    .withConnectionTimeout(40, TimeUnit.SECONDS)
    .withMaxConnectionLifetime(60, TimeUnit.MINUTES)
    .withConnectionAcquisitionTimeout(5, TimeUnit.SECONDS)
    .withMaxConnectionPoolSize(1000)
    .build()

  private lazy val driver: Driver = GraphDatabase.driver(dbUri, AuthTokens.basic(dbUser, dbPass), driverConfig)

  def buildDependencyGraph(): Unit = {
    var batchCount = 0
    var lastBatchSize = 1

    driver.verifyConnectivity()

    while(lastBatchSize > 0){
      lastBatchSize = handleBatch(batchCount)
      batchCount += 1
    }
  }

  private def handleBatch(batchNumber: Int): Int = {

    val httpClient: CloseableHttpClient = HttpClients.createDefault()

    val result = Try {
      logger.info(s"Handling batch #$batchNumber")

      val gavsForBatch = getGAVBatch(batchNumber, httpClient)

      val dependencyData = gavsForBatch
        .flatMap { gav =>
          Try(getDirectDependencies(gav, httpClient)) match {
            case Success(deps) =>
              Some(ProgramWithDependencies(gav, deps))
            case Failure(ex) =>
              logger.error(s"Failed to get dependencies for $gav", ex)
              None
          }
        }

      Try(storeBatch(dependencyData)) match {
        case Success(_) =>
          logger.info(s"Successfully stored ${dependencyData.size} artifacts for batch $batchNumber")
        case Failure(ex) =>
          logger.error(s"Failure while storing data for batch $batchNumber", ex)
      }


      gavsForBatch.size
    }

    httpClient.close()

    result match {
      case Success(cnt) => cnt
      case Failure(ex) =>
        logger.error(s"Unexpected exception handling batch #$batchNumber", ex)
        1 // Return a number > 0 so this does not lead to graph generation being aborted
    }
  }

  private def storeBatch(data: Seq[ProgramWithDependencies]): Unit = {
    val edgeBatchSize = 1000

    val session = driver.session()

    val result = Try {
      val gavProps = new util.HashMap[java.lang.String, AnyRef]()
      gavProps.put("gavs", data.map(_.gav).toList.asJava)

      session.run(cypherArtifactBatchInsert, gavProps)

      val edges = data.flatMap(p => p.dependencyGAVs.map(d => (p.gav, d)))

      edges.grouped(edgeBatchSize).foreach { edgeBatch =>
        val depProps = new util.HashMap[java.lang.String, AnyRef]()
        depProps.put("deps", edgeBatch.map{ t =>
          val tmpMap = new util.HashMap[java.lang.String, java.lang.String]()
          tmpMap.put("source", t._1)
          tmpMap.put("target", t._2)
          tmpMap
        }.toList.asJava)

        session.run(cypherEdgeBatchInsert, depProps)
      }
    }

    session.close()

    if(result.isFailure) throw result.failed.get
  }

  private def getGAVBatch(n: Int, httpClient: CloseableHttpClient): List[String] = {
    val request = new HttpGet(apiBaseUrl + "entities?kind=Program&language=Java")
    request.setHeader("limit", String.valueOf(batchSize))
    request.setHeader("skip", String.valueOf(n * batchSize))

    val response = httpClient.execute(request)

    if (response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve $n th batch of GAVs (Status code ${response.getStatusLine.getStatusCode}) ")
    }

    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    contentT.get match {
      case JsArray(values) =>
        values.collect {
          case jo: JsObject if jo.fields.contains("name") =>
            jo.fields("name").asInstanceOf[JsString].value
        }.toList

      case _ =>
        throw new IllegalStateException("Invalid response format")
    }
  }

  private def getDirectDependencies(artifactGav: String, httpClient: CloseableHttpClient): Set[String] = {
    val entityIdent = gavToEntityId(artifactGav)

    getAnalysisResultsForEntity(entityIdent, "mvn-dependencies", "1.0.0", apiBaseUrl, httpClient) match {
      case Success(jsValue) if jsValue.isInstanceOf[JsArray] =>
        val resultJson = jsValue.asInstanceOf[JsArray]
        val dependencyGAVs = resultJson.elements.map {
          case obj: JsObject =>
            val g = obj.fields("identifier").asJsObject.fields("groupId").asInstanceOf[JsString].value
            val a = obj.fields("identifier").asJsObject.fields("artifactId").asInstanceOf[JsString].value
            val v = obj.fields("identifier").asJsObject.fields("version").asInstanceOf[JsString].value
            g + ":" + a + ":" + v
          case _ =>
            throw new IllegalStateException("Malformed results")
        }

        dependencyGAVs.toSet
      case Failure(ex) =>
        throw ex
      case _ =>
        throw new IllegalStateException("Invalid result format")
    }
  }
}
