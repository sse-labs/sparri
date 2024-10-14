package org.anon.spareuse.mvnem.storage

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.util.Try

trait EntityMinerStorageAdapter {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  def storeJavaProgram(data: JavaProgram): Future[String]

  def ensureProgramNotPresent(programGav: String): Unit

  def hasProgram(gav: String): Boolean

  def initialize(): Unit

  def shutdown(): Unit

}
