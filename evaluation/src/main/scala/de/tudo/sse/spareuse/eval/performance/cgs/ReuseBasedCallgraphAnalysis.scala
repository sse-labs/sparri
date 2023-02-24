package de.tudo.sse.spareuse.eval.performance.cgs
import de.tudo.sse.spareuse.eval.performance.{gavToEntityId, getAnalysisResultsForEntity, runFinished, triggerAnalysisRun}
import org.apache.http.impl.client.HttpClients
import spray.json.JsObject

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ReuseBasedCallgraphAnalysis(apiBaseUrl: String) extends WholeProgramCgAnalysis {

  private val httpClient = HttpClients.createDefault()
  private val analysisName = "mvn-partial-callgraphs"
  private val analysisVersion = "1.0.0"

  private var rootCgJson: Option[JsObject] = None
  private var dependencyCgsJson = mutable.HashSet[JsObject]()

  override def prepareData(rootGav: String, dependencyGavs: Set[String]): Try[Unit] = Try {

    getAnalysisResultsForEntity(gavToEntityId(rootGav), analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
      case jo: JsObject => rootCgJson = Some(jo)
      case _ =>
        throw new IllegalStateException("Expected a JSON Object")
    }

    dependencyGavs
      .map(gavToEntityId)
      .foreach{ entityId =>
        getAnalysisResultsForEntity(entityId, analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
          case jo: JsObject => dependencyCgsJson.add(jo)
          case _ =>
            throw new IllegalStateException("Expected a JSON Object")
        }
      }

    logger.info(s"Successfully retrieved ${dependencyGavs.size + 1} partial callgraphs")


  }

  override def buildFullCallgraph(): Try[Any] = Try{null}//TODO!


  def ensureAllPartialResultsPresent(allGavs: Set[String]): Try[Unit] = Try {
    triggerAnalysisRun(allGavs.map(gavToEntityId), analysisName, analysisVersion, apiBaseUrl, httpClient) match {
      case Success(runLocation) =>
        logger.info(s"Successfully triggered analysis run. Waiting for run at $runLocation to complete...")

        while(!runFinished(runLocation, apiBaseUrl, httpClient).get){
          Thread.sleep(1000)
          logger.debug(s"Waiting for run to finish: $runLocation ...")
        }

        logger.info("All partial callgraphs are available.")
      case Failure(ex) =>
        logger.error("Failed to trigger analysis run for partial callgraphs", ex)
    }
  }

  def close(): Unit = httpClient.close()

  def cleanup(): Unit = {
    rootCgJson = None
    dependencyCgsJson = new mutable.HashSet[JsObject]()
  }
}
