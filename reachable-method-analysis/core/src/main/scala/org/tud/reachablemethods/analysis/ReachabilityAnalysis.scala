package org.tud.reachablemethods.analysis

import org.tud.reachablemethods.analysis.logging.{AnalysisLogger, AnalysisLogging}
import org.tud.reachablemethods.analysis.model.{ClassList, MavenIdentifier}
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import java.io.File
import scala.util.{Failure, Success, Try}

trait ReachabilityAnalysis extends AnalysisLogging {

  protected val log: AnalysisLogger
  OPALProjectHelper.initializeLogging(log)

  def shutdown(): Unit = {}

  def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean

  def analyzeProject(projectClasses: ClassList,
                     dependencies: Iterable[MavenIdentifier],
                     treatProjectAsLibrary: Boolean = false): Try[Set[String]]

  def analyzeMavenProject(classDir: File,
                          dependencies: Iterable[MavenIdentifier],
                          treatProjectAsLibrary: Boolean = false): Try[Set[String]] = {

    ClassList.readClassesFromDirectory(classDir, loadImplementation = true, recurse = true) match {
      case Success(classList) =>
        analyzeProject(classList, dependencies, treatProjectAsLibrary)
      case Failure(ex) =>
        log.error("Failed to load project classes from: " + classDir.getPath, ex)
        Failure(ex)
    }
  }

}
