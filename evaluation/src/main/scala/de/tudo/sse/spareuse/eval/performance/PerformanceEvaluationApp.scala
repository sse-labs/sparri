package de.tudo.sse.spareuse.eval.performance

import de.tudo.sse.spareuse.eval.performance.cgs.CallgraphPerformanceEvaluation
import de.tudo.sse.spareuse.eval.performance.dependencies.DependencyPerformanceEvaluation
import de.tudo.sse.spareuse.eval.triggerEntityMining
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object PerformanceEvaluationApp {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val apiBaseUrl = "http://ls5vs029.cs.tu-dortmund.de:9090/api/"

  private val allRegisteredEvaluations: Seq[PerformanceEvaluation]= Seq(//new DependencyPerformanceEvaluation(apiBaseUrl))
    new CallgraphPerformanceEvaluation(apiBaseUrl))


  private def ensureAllEntitiesPresent(): Try[Unit] = {
    val allRequiredEntityIds = allRegisteredEvaluations.flatMap(_.requiredEntityIds).toSet

    val client = HttpClients.createDefault()

    def checkEntityPresent(entityId: String): Boolean = {
      val request = new HttpGet(apiBaseUrl + "/entities/" + entityId)
      val response = client.execute(request)

      val statusCode = response.getStatusLine.getStatusCode

      EntityUtils.consume(response.getEntity)
      response.close()
      statusCode == 200
    }

    val result = Try {
      val entitiesToAwait = mutable.HashSet[String]()

      allRequiredEntityIds
        .filter(triggerEntityMining(_, apiBaseUrl, client).isEmpty)
        .foreach(entitiesToAwait.add)

      val present = allRequiredEntityIds.size - entitiesToAwait.size
      logger.info(s"$present required entities present, waiting for ${entitiesToAwait.size} to be indexed...")

      while(entitiesToAwait.nonEmpty){
        Try(Thread.sleep(1000))
        val found = entitiesToAwait.filter(id => checkEntityPresent(id))
        found.foreach(entitiesToAwait.remove)
        logger.info(s"Waiting for ${entitiesToAwait.size} entities to be indexed...")
      }

      logger.info(s"All ${allRequiredEntityIds.size} required entities indexed.")
    }

    client.close()
    result
  }

  private def runEval(evaluation: PerformanceEvaluation): Unit = {
    logger.info(s"Running ${evaluation.name} evaluation with ${evaluation.numberOfRepetitions} repetitions...")

    evaluation.evaluate() match {
      case Success(_) =>
        logger.info(s"Execution of ${evaluation.name} successful.")
      case Failure(ex) =>
        logger.error(s"Failed to run performance evaluation ${evaluation.name}", ex)

    }
  }

  def main(args: Array[String]): Unit = {
    logger.info("[Performance Evaluation of SPAR]")
    logger.info(s"${allRegisteredEvaluations.size} individual evaluation(s) registered: [${allRegisteredEvaluations.map(_.name).mkString(",")}]")
    logger.info("")
    logger.info("Making sure all evaluation entities are indexed...")
    ensureAllEntitiesPresent() match {
      case Success(_) =>
        logger.info("All required entities indexed.")
        logger.info("")
        allRegisteredEvaluations.foreach(runEval)
        logger.info("Finished performance evaluation of SPAR.")
      case Failure(ex) =>
        logger.error("Failed to ensure entities are indexed", ex)
    }

  }

}
