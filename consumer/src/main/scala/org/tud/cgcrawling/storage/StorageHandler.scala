package org.tud.cgcrawling.storage

import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.model.LibraryCallGraphEvolution


trait StorageHandler {

  protected val log: Logger = LoggerFactory.getLogger(this.getClass)

  def storeCallGraphEvolution(cgEvolution: LibraryCallGraphEvolution): GraphDbStorageResult

  def libraryExists(libName: String): Option[Boolean]

}

case class GraphDbStorageResult(libraryName: String, success: Boolean)
