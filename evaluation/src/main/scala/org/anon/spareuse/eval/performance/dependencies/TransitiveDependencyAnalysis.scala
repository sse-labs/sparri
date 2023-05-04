package org.anon.spareuse.eval.performance.dependencies

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

trait TransitiveDependencyAnalysis {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
   * Given the GAV of an artifact, calculate all dependency GAVs
   * @param rootArtifactGAV GAV of root artifact
   * @return Set of all dependencies
   */
  def getAllDependencies(rootArtifactGAV: String): Try[Set[String]]



}
