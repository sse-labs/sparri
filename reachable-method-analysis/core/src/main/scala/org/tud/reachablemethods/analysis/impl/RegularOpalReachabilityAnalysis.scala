package org.tud.reachablemethods.analysis.impl

import org.tud.reachablemethods.analysis.ReachabilityAnalysis
import org.tud.reachablemethods.analysis.impl.callgraphs.RegularCallGraphBuilder
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.model.MavenIdentifier
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import scala.util.Try

class RegularOpalReachabilityAnalysis extends ReachabilityAnalysis{
  override protected val loadDependencyImplementations: Boolean = true

  // No need to check index or DB, since dependencies are resolved beforehand!
  override def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean = true


  override def analyzeProject(projectClasses: ClassList, dependencyClasses: ClassList, classFqnDependencyLookup: Map[String, MavenIdentifier], treatProjectAsLibrary: Boolean): Try[Any] = {

    log.info("Initializing OPAL analysis infrastructure..")
    val opalProject = OPALProjectHelper.buildOPALProject(projectClasses, dependencyClasses, treatProjectAsLibrary)
    log.info("Done Initializing OPAL.")

    val builder = new RegularCallGraphBuilder(opalProject)

    builder.calculateMethodsReachable()
  }
}