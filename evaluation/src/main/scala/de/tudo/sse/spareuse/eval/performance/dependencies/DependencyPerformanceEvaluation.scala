package de.tudo.sse.spareuse.eval.performance.dependencies

import de.tudo.sse.spareuse.eval.performance.timedExec
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}
import scala.collection.mutable

object DependencyPerformanceEvaluation extends App {

  val logger = LoggerFactory.getLogger(getClass)
  val numberOfRepetitions = 5


  val gavToDependenciesMap: Map[String, Set[String]] = Map("org.gwtproject.core:gwt-core:1.0.0-RC1" -> Set("com.google.jsinterop:jsinterop-annotations:1.0.2", "com.google.elemental2:elemental2-core:1.1.0", "com.google.elemental2:elemental2-promise:1.1.0", "com.google.elemental2:elemental2-dom:1.1.0", "com.google.jsinterop:jsinterop-annotations:2.0.0", "com.google.jsinterop:base:1.0.0"))

  val simpleAnalysis = new SimpleTransitiveDependencyAnalysis
  val reuseAnalysis = new ReuseBasedTransitiveDependencyAnalysis("")//TODO: URL for test instance

  val preparePartialResultsOp = timedExec (() => Try (reuseAnalysis.ensureAllPartialResultsPresent((gavToDependenciesMap.keys ++ gavToDependenciesMap.values.flatten).toSet)))

  preparePartialResultsOp.getContent match {
    case Success(_) =>
      logger.info(s"Reuse: Pre-Calculations took ${preparePartialResultsOp.getDurationMillis}ms to finish.")
      compareAnalyses()
    case Failure(ex) =>
      logger.error("Failed to trigger initial calculation of partial dependencies on server side.", ex)
  }

  private def compareAnalyses(): Unit ={
    val simpleRuntimes = new mutable.ListBuffer[Long]()
    val reuseRuntimes = new mutable.ListBuffer[Long]()

    for (_ <- Range(0, numberOfRepetitions)) {
      gavToDependenciesMap.keys.foreach { input =>

        val timedSimpleResults = timedExec(() => simpleAnalysis.getAllDependencies(input))
        val timedReuseResults = timedExec(() => reuseAnalysis.getAllDependencies(input))

        reuseRuntimes.append(timedReuseResults.getDurationMillis)

        if (timedSimpleResults.getContent.isFailure || !timedSimpleResults.getContent.get.equals(gavToDependenciesMap(input)))
          logger.error("Invalid results for simple dependency analysis")
        else
          simpleRuntimes.append(timedSimpleResults.getDurationMillis)

        /*if(timedReuseResults._2.isFailure || ! timedReuseResults._2.get.equals(gavToDependenciesMap(input)))
          logger.error("Invalid results for reuse dependency analysis")
        else
          reuseRuntimes.add(timedReuseResults._1)*/
      }

    }

    logger.info(s"Got ${simpleRuntimes.size} valid simple runtimes and ${reuseRuntimes.size} valid reuse runtimes.")

    if (simpleRuntimes.nonEmpty) {
      val simpleAverageMillis = simpleRuntimes.sum / simpleRuntimes.size
      logger.info(s"Simple: AVG $simpleAverageMillis ms [${simpleRuntimes.map(r => s"$r ms").mkString(",")}]")
    }

    if (reuseRuntimes.nonEmpty) {
      val reuseAverageMillis = reuseRuntimes.sum / reuseRuntimes.size
      logger.info(s"Reuse: AVG $reuseAverageMillis ms [${reuseRuntimes.map(r => s"$r ms").mkString(",")}]")
    }
  }






}
