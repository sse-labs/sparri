package org.tud.reachablemethods.analysis

import org.slf4j.{Logger, LoggerFactory}
import org.tud.reachablemethods.analysis.model.{ClassContainerFile, ClassList, MavenIdentifier, MavenJarFileDependency}
import org.tud.reachablemethods.analysis.model.ClassList.ClassList

import java.io.File
import scala.util.Try

trait ReachabilityAnalysis {

  protected val log: Logger = LoggerFactory.getLogger(this.getClass)

  def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean

  def analyzeProject(projectClasses: ClassList, dependencyClasses: ClassList, classFqnDependencyLookup: Map[String, MavenIdentifier], treatProjectAsLibrary: Boolean = false): Try[Any]

  def analyzeMavenProject(classDir: File, dependencies: Iterable[MavenJarFileDependency], treatProjectAsLibrary: Boolean = false): Try[Any] = {

    val projectClasses = ClassList.readClassesFromDirectory(classDir, loadImplementation = true, recurse = true)

    val dependencyClasses = Try(dependencies
      .flatMap(_.classContainer.getClassList(loadImplementation = false).get)
      .toList)

    if(projectClasses.isFailure){
      log.error("Failed to load project classes from: " + classDir.getPath, projectClasses.failed.get)
      projectClasses
    } else if(dependencyClasses.isFailure){
      log.error("Failed to load project dependency classes.", dependencyClasses.failed.get)
      dependencyClasses
    } else {

      val lookup = dependencies
        .flatMap{ dep =>
          dep.classContainer.getClassList(loadImplementation = false).get.map(cf => (cf._1.fqn, dep.dependencyIdentifier))
        }.toMap

      analyzeProject(projectClasses.get, dependencyClasses.get, lookup, treatProjectAsLibrary)
    }

  }

}
