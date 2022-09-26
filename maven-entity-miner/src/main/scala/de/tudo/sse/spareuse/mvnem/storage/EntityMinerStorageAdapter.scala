package de.tudo.sse.spareuse.mvnem.storage

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

trait EntityMinerStorageAdapter {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  def storeJavaProgram(data: JavaProgram): Future[_]

  def hasEntityQualifier(fq: String): Boolean

  def initialize(): Unit

  def shutdown(): Unit

}
