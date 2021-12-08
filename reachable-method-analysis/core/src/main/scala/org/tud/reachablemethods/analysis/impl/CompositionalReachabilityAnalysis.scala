package org.tud.reachablemethods.analysis.impl

import akka.actor.ActorSystem
import org.tud.reachablemethods.analysis.dataaccess.MethodDataAccessor
import org.tud.reachablemethods.analysis.{Configuration, ReachabilityAnalysis}
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.model.MavenIdentifier
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import scala.util.{Failure, Try}

class CompositionalReachabilityAnalysis(configuration: Configuration)(implicit system: ActorSystem) extends ReachabilityAnalysis {

  private[impl] val methodAccessor: MethodDataAccessor = new MethodDataAccessor(configuration)
  methodAccessor.initialize()



  override def analyzeProject(projectClasses: ClassList, dependencyClasses: ClassList,
                              classFqnDependencyLookup: Map[String, MavenIdentifier], treatProjectAsLibrary: Boolean): Try[Any] = {

    val allDependencies = classFqnDependencyLookup.values.toList.distinct

    if(!analysisPossible(allDependencies)){
      Failure(new IllegalStateException("Cannot perform reachability analysis: requirements not satisfied"))
    }

    log.info("Initializing OPAL analysis infrastructure..")
    val opalProject = OPALProjectHelper.buildOPALProject(projectClasses, dependencyClasses, treatProjectAsLibrary)
    log.info("Done Initializing OPAL.")

    val analysisContext = new CompositionalAnalysisContext(opalProject, classFqnDependencyLookup)

    allDependencies.foreach { dependency =>
      analysisContext.indexInstantiatedTypes(methodAccessor.getArtifactMetadata(dependency.libraryIdentifier,
        dependency.version).map(_.instantiatedTypes).get) //TODO: Error Handling

      analysisContext.indexMethods(methodAccessor
        .getArtifactMethods(dependency.libraryIdentifier, dependency.version)
        .get) //TODO: Error Handling

    }


    ???
  }

  override def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean = {
    dependencies.forall(dep => methodAccessor.libraryInIndex(dep.libraryIdentifier))
  }
}
