package de.tudo.sse.spareuse.eval.performance.dependencies

import de.tudo.sse.spareuse.eval.performance.{gavToEntityId, getAnalysisResultsForEntity, runFinished, triggerAnalysisRun}
import org.apache.http.impl.client.HttpClients
import spray.json._

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

    getAnalysisResultsForEntity(entityIdent, "mvn-dependencies", "1.0.0", baseUrl, httpClient) match {
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


  def ensureAllPartialResultsPresent(expectedGAVs: Set[String]): Unit = {

    val expectedIds = expectedGAVs.map(gavToEntityId)


    triggerAnalysisRun(expectedIds, "mvn-dependencies", "1.0.0", baseUrl, httpClient) match {
      case Success(runLocation) =>
        logger.info(s"Successfully triggered analysis run. Waiting for run at $runLocation to complete...")

        while(!runFinished(runLocation, baseUrl, httpClient).get) {
          Thread.sleep(500)
          logger.debug(s"Waiting for run to finish: $runLocation")
        }

        logger.info("All direct dependencies are available.")

      case Failure(ex) =>
        logger.error("Failed to trigger analysis run for direct dependencies", ex)
    }
  }

  def close(): Unit = httpClient.close()

}
