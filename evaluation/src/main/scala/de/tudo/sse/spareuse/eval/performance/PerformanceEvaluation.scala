package de.tudo.sse.spareuse.eval.performance

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

trait PerformanceEvaluation {

  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  val name: String

  val numberOfRepetitions: Int


  def requiredEntityIds: Set[String]


  def evaluate(): Try[Unit]


}
