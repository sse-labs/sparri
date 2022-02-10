package org.tud.reachablemethods.analysis.impl

import akka.actor.ActorSystem
import org.tud.reachablemethods.analysis.dataaccess.MethodDataAccessor
import org.tud.reachablemethods.analysis.impl.callgraphs.{CallGraphBuilder, CompositionalCallGraphBuilder}
import org.tud.reachablemethods.analysis.logging.AnalysisLogger
import org.tud.reachablemethods.analysis.{Configuration, ReachabilityAnalysis}
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.model.MavenIdentifier
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import scala.util.{Failure, Success, Try}

class CompositionalReachabilityAnalysis(configuration: Configuration, override val log: AnalysisLogger = new AnalysisLogger) extends ReachabilityAnalysis {

  private val system: ActorSystem = ActorSystem("reachability-analysis")

  private[impl] val methodAccessor: MethodDataAccessor = new MethodDataAccessor(configuration, log)(system)

  methodAccessor.initialize()

  override def analyzeProject(projectClasses: ClassList,
                              dependencies: Iterable[MavenIdentifier],
                              treatProjectAsLibrary: Boolean = false): Try[Set[String]] = {

    if(!analysisPossible(dependencies)){
      Failure(new IllegalStateException("Cannot perform reachability analysis: requirements not satisfied"))
    } else {
      log.info("Initializing OPAL analysis infrastructure..")

      // We always load the local JRE, and no further dependency classes. The local JRE should correspond to the analyses-time JRE
      val opalProject = OPALProjectHelper.buildOPALProject(projectClasses, List.empty, treatProjectAsLibrary)
      log.info("Done Initializing OPAL.")

      val startDownload = System.currentTimeMillis()

      // Context will initialize itself. In that, it will download all methods and types for all dependencies.
      val analysisContext = new CompositionalAnalysisContext(dependencies, methodAccessor, opalProject, log)

      val downloadDuration = (System.currentTimeMillis() - startDownload) / 1000
      val startAnalysis = System.currentTimeMillis()

      val cgBuilder = new CompositionalCallGraphBuilder(opalProject, analysisContext, log)

      cgBuilder.buildCallGraph()

      val analysisDuration = (System.currentTimeMillis() - startAnalysis) / 1000


      methodAccessor.shutdown()

      log.info(s"Compositional analysis finished [download=$downloadDuration s; analysis=$analysisDuration s]")

      Success(analysisContext.methodSignaturesSeen)
    }

  }

  override def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean = {
    dependencies.forall(dep => methodAccessor.libraryInIndex(dep.libraryIdentifier))
  }

  override def shutdown(): Unit = system.terminate()

}
