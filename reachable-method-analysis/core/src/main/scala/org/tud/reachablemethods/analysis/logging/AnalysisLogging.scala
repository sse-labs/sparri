package org.tud.reachablemethods.analysis.logging

trait AnalysisLogging {

  protected implicit val classVal: Class[_] = getClass

  protected val log: AnalysisLogger

}
