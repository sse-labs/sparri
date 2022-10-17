package de.tudo.sse.spareuse.execution.analyses

import de.tudo.sse.spareuse.core.model.AnalysisRunData
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

/**
 * TODO: Separate info on analysis RUN and analysis Impl
 * @param rawConfig
 */
abstract class AnalysisImplementation(rawConfig: String) {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  val name: String
  val version: String

  val inputEntityKind: SoftwareEntityKind

  def executionPossible(inputs: Seq[SoftwareEntityData]): Boolean

  def executeAnalysis(inputs: Seq[SoftwareEntityData]): Try[AnalysisRunData]

  def stopExecution(): Unit

}
