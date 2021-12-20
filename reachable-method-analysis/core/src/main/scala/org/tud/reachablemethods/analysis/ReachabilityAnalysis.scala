package org.tud.reachablemethods.analysis

import org.tud.reachablemethods.analysis.logging.{AnalysisLogger, AnalysisLogging}
import org.tud.reachablemethods.analysis.model.{ClassList, MavenIdentifier, MavenJarFileDependency}
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import java.io.File
import scala.util.{Failure, Try}

trait ReachabilityAnalysis extends AnalysisLogging {

  protected val log: AnalysisLogger
  OPALProjectHelper.initializeLogging(log)

  protected val loadDependencyImplementations: Boolean

  def shutdown(): Unit = {}

  def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean

  def analyzeProject(projectClasses: ClassList, dependencyClasses: ClassList, classFqnDependencyLookup: Map[String, MavenIdentifier], treatProjectAsLibrary: Boolean = false): Try[Set[String]]

  def analyzeMavenProject(classDir: File, dependencies: Iterable[MavenJarFileDependency], treatProjectAsLibrary: Boolean = false): Try[Set[String]] = {

    val projectClasses = ClassList.readClassesFromDirectory(classDir, loadImplementation = true, recurse = true)

    val dependencyClasses = Try(dependencies
      .flatMap(_.classContainer.getClassList(loadImplementation = loadDependencyImplementations).get)
      .toList)

    if(projectClasses.isFailure){
      log.error("Failed to load project classes from: " + classDir.getPath, projectClasses.failed.get)
      Failure(projectClasses.failed.get)
    } else if(dependencyClasses.isFailure){
      log.error("Failed to load project dependency classes.", dependencyClasses.failed.get)
      Failure(dependencyClasses.failed.get)
    } else {

      val lookup = dependencies
        .flatMap{ dep =>
          dep.classContainer.getClassList(loadImplementation = false).get.map(cf => (cf._1.fqn, dep.dependencyIdentifier))
        }.toMap

      analyzeProject(projectClasses.get, dependencyClasses.get, lookup, treatProjectAsLibrary)
    }

  }

}
