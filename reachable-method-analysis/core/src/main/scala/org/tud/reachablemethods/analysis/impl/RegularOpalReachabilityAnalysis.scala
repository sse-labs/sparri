package org.tud.reachablemethods.analysis.impl

import org.tud.reachablemethods.analysis.ReachabilityAnalysis
import org.tud.reachablemethods.analysis.impl.callgraphs.RegularCallGraphBuilder
import org.tud.reachablemethods.analysis.logging.AnalysisLogger
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.model.MavenIdentifier
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import scala.util.Try

class RegularOpalReachabilityAnalysis(dependencyClasses: ClassList, override val log: AnalysisLogger = new AnalysisLogger) extends ReachabilityAnalysis {

  // No need to check index or DB, since dependencies are resolved beforehand!
  override def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean = true


  override def analyzeProject(projectClasses: ClassList, dependencies: Iterable[MavenIdentifier], treatProjectAsLibrary: Boolean): Try[Set[String]] = {

    log.info("Initializing OPAL analysis infrastructure..")
    val opalProject = OPALProjectHelper.buildOPALProject(projectClasses, dependencyClasses, treatProjectAsLibrary)
    log.info("Done Initializing OPAL.")

    val builder = new RegularCallGraphBuilder(opalProject, log)

    builder.calculateMethodsReachable().map(_.toSet)
  }
}
