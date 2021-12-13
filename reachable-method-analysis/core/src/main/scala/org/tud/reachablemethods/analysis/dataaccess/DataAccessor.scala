package org.tud.reachablemethods.analysis.dataaccess

import org.slf4j.{Logger, LoggerFactory}
import org.tud.reachablemethods.analysis.Configuration

abstract class DataAccessor(config: Configuration) {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  def initialize(): Unit

  def shutdown(): Unit


}
