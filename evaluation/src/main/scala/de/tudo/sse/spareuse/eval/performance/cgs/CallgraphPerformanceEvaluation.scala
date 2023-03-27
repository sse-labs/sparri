package de.tudo.sse.spareuse.eval.performance.cgs

import de.tudo.sse.spareuse.eval.performance.{PerformanceEvaluation, gavToEntityId, timedExec}
import org.opalj.tac.cg.CallGraph

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CallgraphPerformanceEvaluation(apiBaseUrl: String) extends PerformanceEvaluation {

  val depMaps: Map[String, Set[String]] = Map(
    "org.gwtproject.core:gwt-core:1.0.0-RC1" ->
      Set("com.google.jsinterop:jsinterop-annotations:1.0.2", "com.google.elemental2:elemental2-core:1.1.0", "com.google.elemental2:elemental2-promise:1.1.0", "com.google.elemental2:elemental2-dom:1.1.0", "com.google.jsinterop:jsinterop-annotations:2.0.0", "com.google.jsinterop:base:1.0.0"),
    "org.springframework:spring-context:5.3.3" ->
      Set("org.springframework:spring-aop:5.3.3", "org.springframework:spring-beans:5.3.3", "org.springframework:spring-jcl:5.3.3", "org.springframework:spring-core:5.3.3", "org.springframework:spring-expression:5.3.3"),
  )


  override val name: String = "WholeProgramCallgraphs"
  override val numberOfRepetitions: Int = 1

  override def requiredEntityIds: Set[String] = depMaps.flatMap(t => Set(t._1) ++ t._2).toSet.map(gavToEntityId)

  private val directAnalysis = new DirectCallgraphAnalysis
  private val reuseAnalysis = new ReuseBasedCallgraphAnalysis(apiBaseUrl)

  override def evaluate(): Try[Unit] = {

    val preparePartialResultsOp = timedExec(() => reuseAnalysis.ensureAllPartialResultsPresent(depMaps.flatMap(t => Set(t._1) ++ t._2).toSet))

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

      depMaps.foreach { t =>
        val rootGav = t._1
        val dependencyGAVs = t._2

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

        var allSimple: Set[String] = Set.empty

        if (timedSimpleResults.getContent.isFailure)
          logger.error("Invalid results for simple dependency analysis")
        else {
          val result: CallGraph = timedSimpleResults.getContent.get
          allSimple = result.reachableMethods()
            .filterNot(_.declaringClassType.fqn.startsWith("java/lang/"))
            .filterNot(_.declaringClassType.fqn.startsWith("java/util/"))
            .map(m => s"${m.declaringClassType.fqn.replace("/", ".")} ${m.name} ${m.descriptor.parameterTypes.map(_.toJava).mkString(",")}").toSet
          directRuntimes.append(timedSimpleResults.getDurationMillis)
        }

        if (timedReuseResults.getContent.isFailure)
          logger.error("Invalid results for reuse dependency analysis", timedReuseResults.getContent.failed.get)
        else {
          val result: reuseAnalysis.StitchedCallGraph = timedReuseResults.getContent.get
          val allReuse = result.reachableMethods().map(m => s"${m.identifier.mDeclaringTypeFqn} ${m.identifier.mName} ${m.identifier.mArgumentTypes.mkString(",")}")

          val d1 = allSimple.diff(allReuse)
          val d2 = allReuse.diff(allSimple)
          val d3 = allSimple.intersect(allReuse)

          logger.info(s"Simple: [Total: ${allSimple.size}, Unique: ${d1.size}], Reuse: [Total: ${allReuse.size}, Unique: ${d2.size}], Common: ${d3.size}")

          if(d1.size > 50){
            logger.info("Unique Simple:")
            d1.foreach(s => logger.info(s"\t - $s"))
          }

          if(d2.size > 50){
            logger.info("Unique Reuse:")
            d2.foreach(s => logger.info(s"\t - $s"))
          }

          reuseRuntimes.append(timedReuseResults.getDurationMillis)
        }

        directAnalysis.cleanup()
        reuseAnalysis.cleanup()
      }
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
