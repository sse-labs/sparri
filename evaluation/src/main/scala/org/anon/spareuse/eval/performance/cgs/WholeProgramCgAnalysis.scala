package org.anon.spareuse.eval.performance.cgs

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

trait WholeProgramCgAnalysis {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  def prepareData(rootGav: String, dependencyGavs: Set[String]): Try[Unit]

  def buildFullCallgraph(): Try[Any] //TODO: Type

  def cleanup(): Unit

}
