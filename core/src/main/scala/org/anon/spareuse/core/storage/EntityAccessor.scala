package org.anon.spareuse.core.storage

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities._
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.GenericEntityData

import scala.annotation.switch
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

  def getLibraryEntityId(ga: String): Option[Long]
  def getProgramEntityId(gav: String): Option[Long]
  def getPackageEntityId(gav: String, pName: String): Option[Long]
  def getClassEntityId(gav: String, classFqn: String): Option[Long]
  def getMethodEntityId(gav: String, classFqn: String, methodIdent: String): Option[Long]
  def getStatementEntityId(gav: String, classFqn: String, methodIdent: String, pcIdent: String): Option[Long]

  def hasProgram(gav: String): Boolean = getProgramEntityId(gav).isDefined

  def getEntityIdFor(identifiers: Seq[String]): Option[Long] = (identifiers.length : @switch)match {
    case 1 => getLibraryEntityId(identifiers.head)
    case 2 => getProgramEntityId(s"${identifiers.head}:${identifiers(1)}")
    case 3 => getPackageEntityId(s"${identifiers.head}:${identifiers(1)}", identifiers(2))
    case 4 => getClassEntityId(s"${identifiers.head}:${identifiers(1)}", identifiers(3))
    case 5 => getMethodEntityId(s"${identifiers.head}:${identifiers(1)}", identifiers(3), identifiers(4))
    case 6 => getStatementEntityId(s"${identifiers.head}:${identifiers(1)}", identifiers(3), identifiers(4), identifiers(5))
    case _ => None
  }

}
