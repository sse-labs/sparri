package de.tudo.sse.spareuse.eval.performance.cgs

import de.tudo.sse.spareuse.eval.performance.{PerformanceEvaluation, gavToEntityId, timedExec}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CallgraphPerformanceEvaluation(apiBaseUrl: String) extends PerformanceEvaluation {

  private val rootGav = "org.gwtproject.core:gwt-core:1.0.0-RC1"
  private val dependencyGAVs = Set("com.google.jsinterop:jsinterop-annotations:1.0.2", "com.google.elemental2:elemental2-core:1.1.0", "com.google.elemental2:elemental2-promise:1.1.0", "com.google.elemental2:elemental2-dom:1.1.0", "com.google.jsinterop:jsinterop-annotations:2.0.0", "com.google.jsinterop:base:1.0.0")

  override val name: String = "WholeProgramCallgraphs"
  override val numberOfRepetitions: Int = 1

  // We base our whole-program CG evaluation on the same program (GWT Core) we use for evaluating dependencies
  override def requiredEntityIds: Set[String] = (Set(rootGav) ++ dependencyGAVs).map(gavToEntityId)

  private val directAnalysis = new DirectCallgraphAnalysis
  private val reuseAnalysis = new ReuseBasedCallgraphAnalysis(apiBaseUrl)

  override def evaluate(): Try[Unit] = {

    val preparePartialResultsOp = timedExec(() => reuseAnalysis.ensureAllPartialResultsPresent(Set(rootGav) ++ dependencyGAVs))

    preparePartialResultsOp.getContent match {
      case Success(_) =>
        logger.info(s"Reuse pre-calculations took ${preparePartialResultsOp.getDurationMillis}ms to finish.")
        compareAnalyses()
      case Failure(ex) =>
        logger.error("Failed to trigger initial calculation of partial callgraphs on server side.", ex)
        Failure(ex)
    }
  }

  private def compareAnalyses(): Try[Unit] = Try {
    val directRuntimes = new mutable.ListBuffer[Long]()
    val reuseRuntimes = new mutable.ListBuffer[Long]()

    for (_ <- Range(0, numberOfRepetitions)) {

      val timedSimpleResults = timedExec(() => {
        directAnalysis.prepareData(rootGav, dependencyGAVs) match {
          case Success(_) =>
            directAnalysis.buildFullCallgraph()
          case Failure(ex) =>
            logger.error("Failed to prepare data for direct CG analysis", ex)
            Failure(ex)
        }
      })

      val timedReuseResults = timedExec(() => {
        reuseAnalysis.prepareData(rootGav, dependencyGAVs) match {
          case Success(_) =>
            reuseAnalysis.buildFullCallgraph()
          case Failure(ex) =>
            logger.error("Failed to prepare data for reuse CG analysis", ex)
            Failure(ex)
        }
      })

      if (timedSimpleResults.getContent.isFailure) //TODO: Check content semantics!
        logger.error("Invalid results for simple dependency analysis")
      else
        directRuntimes.append(timedSimpleResults.getDurationMillis)

      if (timedReuseResults.getContent.isFailure) //TODO: Check content semantics
        logger.error("Invalid results for reuse dependency analysis")
      else
        reuseRuntimes.append(timedReuseResults.getDurationMillis)

      directAnalysis.cleanup()
      reuseAnalysis.cleanup()
    }

    reuseAnalysis.close()

    logger.info(s"Got ${directRuntimes.size} valid simple runtimes and ${reuseRuntimes.size} valid reuse runtimes.")

    if (directRuntimes.nonEmpty) {
      val simpleAverageMillis = directRuntimes.sum / directRuntimes.size
      logger.info(s"Simple: AVG $simpleAverageMillis ms [${directRuntimes.map(r => s"$r ms").mkString(",")}]")
    }

    if (reuseRuntimes.nonEmpty) {
      val reuseAverageMillis = reuseRuntimes.sum / reuseRuntimes.size
      logger.info(s"Reuse: AVG $reuseAverageMillis ms [${reuseRuntimes.map(r => s"$r ms").mkString(",")}]")
    }

  }
}
