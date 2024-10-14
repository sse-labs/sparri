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

  protected val awaitEntityTimeout: FiniteDuration = 300.seconds

  def initializeEntityTables(): Unit

  def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[Long]): Future[Seq[SoftwareEntityData]]

  def getEntityChildren(eid: Long, skip: Int, limit:Int): Try[Seq[SoftwareEntityData]]

  def getEntityKind(eId: Long): Try[SoftwareEntityKind]

  def getEntity(eid: Long, resolutionDepth: Option[Int]): Future[SoftwareEntityData] = {
    if(hasEntity(eid)){

      getEntityKind(eid) match {
        case Success(entityKind) =>
          // If no depth is given, resolve the entire tree
          val resolutionScope = if(resolutionDepth.isDefined)
            SoftwareEntityKind.fromId(Math.min(entityKind.id + resolutionDepth.get, SoftwareEntityKind.InvocationStatement.id))
          else
            SoftwareEntityKind.InvocationStatement

          getEntity(eid, resolutionScope)
        case Failure(ex) =>
          Future.failed(ex)
      }

    } else Future.failed(new IllegalStateException(s"Entity not present: $eid"))
  }

  def awaitGetEntity(eid: Long, resolutionDepth: Option[Int]): Try[SoftwareEntityData] = {
    Try(Await.result(getEntity(eid, resolutionDepth), awaitEntityTimeout))
  }

  def getEntity(eid: Long, resolutionScope: SoftwareEntityKind): Future[SoftwareEntityData]

  def awaitGetEntity(eid: Long, resolutionScope: SoftwareEntityKind): Try[SoftwareEntityData] = {
    Try(Await.result(getEntity(eid, resolutionScope), awaitEntityTimeout))
  }
  def hasEntity(eid: Long, kind: SoftwareEntityKind): Boolean

  def hasEntity(eid: Long): Boolean

  def getProgramEntityId(gav: String): Option[Long]

  def hasProgram(gav: String): Boolean = getProgramEntityId(gav).isDefined

  def getClassEntityId(gav: String, classFqn: String): Option[Long]

  def getMethodEntityId(gav: String, classFqn: String, methodIdent: String): Option[Long]

}
