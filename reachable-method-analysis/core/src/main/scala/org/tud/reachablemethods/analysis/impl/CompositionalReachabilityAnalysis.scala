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

  override protected val loadDependencyImplementations: Boolean = false

  private[impl] val methodAccessor: MethodDataAccessor = new MethodDataAccessor(configuration, log)(system)

  methodAccessor.initialize()

  override def analyzeProject(projectClasses: ClassList, dependencyClasses: ClassList,
                              classFqnDependencyLookup: Map[String, MavenIdentifier], treatProjectAsLibrary: Boolean): Try[Set[String]] = {

    val jreIdent = MavenIdentifier("<none>", "<jre>", methodAccessor.getIndexedJreVersion.get)
    val classFqnLookupWithJRE = classFqnDependencyLookup ++ OPALProjectHelper.jreClasses.map(c => (c._1.fqn, jreIdent)).toMap
    val allDependencies = classFqnLookupWithJRE.values.toList.distinct

    if(!analysisPossible(allDependencies)){
      Failure(new IllegalStateException("Cannot perform reachability analysis: requirements not satisfied"))
    } else {
      log.info("Initializing OPAL analysis infrastructure..")
      val opalProject = OPALProjectHelper.buildOPALProject(projectClasses, dependencyClasses, treatProjectAsLibrary)
      log.info("Done Initializing OPAL.")

      val startDownload = System.currentTimeMillis()
      val analysisContext = new CompositionalAnalysisContext(classFqnLookupWithJRE, methodAccessor, opalProject, log)

      // Add all instantiated types of current project to index
      analysisContext.indexInstantiatedTypes(CallGraphBuilder.getInstantiatedTypeNames(opalProject, projectOnly = true))

      // Add all instantiated types of dependency projects to index
      allDependencies.foreach { dependency =>
        analysisContext.indexInstantiatedTypes(methodAccessor.getArtifactMetadata(dependency.libraryIdentifier,
          dependency.version).map(_.instantiatedTypes).get) //TODO: Error Handling
      }

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
