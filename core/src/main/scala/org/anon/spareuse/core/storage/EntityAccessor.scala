package org.anon.spareuse.core.storage

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities._
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.GenericEntityData

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

trait EntityAccessor {

  protected val awaitEntityTimeout: FiniteDuration = 60.seconds

  def initializeEntityTables(): Unit

  def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[String]): Future[Seq[SoftwareEntityData]]

  def getEntityChildren(uid: String, skip: Int, limit:Int): Try[Seq[SoftwareEntityData]]

  def getEntityKind(entityIdent: String): Try[SoftwareEntityKind]

  def getEntity(ident: String, resolutionDepth: Option[Int]): Future[SoftwareEntityData] = {
    if(hasEntity(ident)){

      getEntityKind(ident) match {
        case Success(entityKind) =>
          // If no depth is given, resolve the entire tree
          val resolutionScope = if(resolutionDepth.isDefined)
            SoftwareEntityKind.fromId(Math.min(entityKind.id + resolutionDepth.get, SoftwareEntityKind.InvocationStatement.id))
          else
            SoftwareEntityKind.InvocationStatement

          getEntity(ident, resolutionScope)
        case Failure(ex) =>
          Future.failed(ex)
      }

    } else Future.failed(new IllegalStateException(s"Entity not present: $ident"))
  }

  def awaitGetEntity(ident: String, resolutionDepth: Option[Int]): Try[SoftwareEntityData] = {
    Try(Await.result(getEntity(ident, resolutionDepth), awaitEntityTimeout))
  }

  def getEntity(ident: String, resolutionScope: SoftwareEntityKind): Future[SoftwareEntityData]

  def awaitGetEntity(ident: String, resolutionScope: SoftwareEntityKind): Try[SoftwareEntityData] = {
    Try(Await.result(getEntity(ident, resolutionScope), awaitEntityTimeout))
  }
  def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean

  def hasEntity(ident: String): Boolean

}
