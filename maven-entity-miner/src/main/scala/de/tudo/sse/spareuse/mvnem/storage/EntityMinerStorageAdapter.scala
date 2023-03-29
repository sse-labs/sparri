package de.tudo.sse.spareuse.mvnem.storage

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future

trait EntityMinerStorageAdapter {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  def storeJavaProgram(data: JavaProgram): Future[_]

  def hasEntityQualifier(fq: String): Boolean

  def hasProgram(gav: String): Boolean = {
    if(gav.count(_ == ':') != 2)
      throw new IllegalStateException(s"Not a valid GAV triple: $gav")

    val parts = gav.split(":")
    val ident = s"${parts(0)}:${parts(1)}!$gav"
    hasEntityQualifier(ident)
  }

  def initialize(): Unit

  def shutdown(): Unit

}
