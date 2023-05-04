package org.anon.spareuse.core.storage

trait DataAccessor extends EntityAccessor  with AnalysisAccessor {

  def initialize(): Unit = {
    this.initializeEntityTables()
    this.initializeAnalysisTables()
  }

  def shutdown(): Unit

}
