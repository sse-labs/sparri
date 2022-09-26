package de.tudo.sse.spareuse.mvnem.storage.impl

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaPackage, JavaProgram}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.storage.postgresql.{SoftwareEntities, SoftwareEntityRepr}
import de.tudo.sse.spareuse.mvnem.storage.EntityMinerStorageAdapter

import scala.util.{Failure, Success, Try}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class PostgresStorageAdapter extends EntityMinerStorageAdapter {

  lazy val db = Database.forConfig("spa-reuse.postgres")
  val entitiesTable = TableQuery[SoftwareEntities]

  lazy val idReturningEntitiesTable = entitiesTable returning entitiesTable.map(_.id)
  lazy val qualifierAndIdReturningEntitiesTable = entitiesTable returning entitiesTable.map(row => (row.qualifier, row.id))

  override def storeJavaProgram(data: JavaProgram): Future[_] =  {

    Try{
      data.getParent.map(p => {
        if (hasEntityQualifier(p.uid)) {
          getIdForQualifier(p.uid)
        } else {
          storeDataAndGetId(p, None)
        }
      })
    } match {
      case Success(parentIdOpt) =>
        storeInternal(data, parentIdOpt)
      case Failure(ex) =>
        log.error(s"Failed to prepare storage of program: ${data.name}", ex)
        Future.failed(ex)
    }




  }

  private def storeInternal(data: JavaProgram, parentIdOpt: Option[Long]): Future[_] = {
    val currentId = storeDataAndGetId(data, parentIdOpt)

    storeProgram(data, currentId)
  }

  private def storeDataAndGetId(data: SoftwareEntityData, parentIdOpt: Option[Long]): Long = {
    //TODO: Maybe batching?
    log.debug(s"Storing entity ${data.name} ...")
    val res = idReturningEntitiesTable +=
      ((0, data.name, data.uid, data.language, data.kind.id, data.repository, parentIdOpt))

    Await.result(db.run(res), 10.seconds)
  }

  private def getIdForQualifier(fq: String): Long = {
    val queryFuture = db.run(entitiesTable.filter( row => row.qualifier === fq).take(1).map(_.id).result)
    Await.result(queryFuture, 10.seconds).head
  }

  private def storeProgram(jp: JavaProgram, programDbId: Long): Future[_] = {

    //TODO: Specialization tables
    val batch = jp.getChildren.map {
      case jm: JavaPackage =>
        toEntityRepr(jm, Some(programDbId))
    }

    val insertAction = qualifierAndIdReturningEntitiesTable ++= batch

    val insertResult = Await.result(db.run(insertAction), 10.seconds)

    val classLookup = jp.getChildren.map(m => (m.uid, m.asInstanceOf[JavaPackage])).toMap

    Future.sequence(insertResult.map(t => storePackage(classLookup(t._1), t._2)))(executor = db.ioExecutionContext, cbf = implicitly)
  }

  private def storePackage(jp: JavaPackage, packageDbId: Long): Future[_] = {
    //TODO: Specialization tables
    val batch = jp.getChildren.map {
      case jm: JavaClass =>
        toEntityRepr(jm, Some(packageDbId))
    }

    val insertAction = qualifierAndIdReturningEntitiesTable ++= batch

    val insertResult = Await.result(db.run(insertAction), 10.seconds)

    val classLookup = jp.getChildren.map(m => (m.uid, m.asInstanceOf[JavaClass])).toMap

    Future.sequence(insertResult.map(t => storeClass(classLookup(t._1), t._2)))(executor = db.ioExecutionContext, cbf = implicitly)
  }

  private def storeClass(jc: JavaClass, classDbId: Long): Future[_] = {

    //TODO: Specialization tables
    val batch = jc.getChildren.map {
      case jm: JavaMethod =>
        toEntityRepr(jm, Some(classDbId))
    }

    val insertAction = qualifierAndIdReturningEntitiesTable ++= batch

    val insertResult = Await.result(db.run(insertAction), 10.seconds)

    val methodLookup = jc.getChildren.map(m => (m.uid, m.asInstanceOf[JavaMethod])).toMap

    Future.sequence(insertResult.map(t => storeMethod(methodLookup(t._1), t._2)))(executor = db.ioExecutionContext, cbf = implicitly)
  }

  private def storeMethod(jm: JavaMethod, methodDbId: Long): Future[_] = {

    //TODO: Specialization tables
    val batch = jm.getChildren.map {
      case jis: JavaInvokeStatement =>
        toEntityRepr(jis, Some(methodDbId))
      case jfas: JavaFieldAccessStatement =>
        toEntityRepr(jfas, Some(methodDbId))
    }

    val action = entitiesTable ++= batch

    db.run(action)
  }

  private def toEntityRepr(data: SoftwareEntityData, parentIdOpt: Option[Long]): SoftwareEntityRepr = (0, data.name,
    data.uid, data.language, data.kind.id, data.repository, parentIdOpt)




  override def initialize(): Unit = {
    val setupAction = entitiesTable.schema.createIfNotExists

    val setupFuture = db.run(setupAction)

    Await.ready(setupFuture, 20.seconds)
  }

  override def shutdown(): Unit = db.close()

  override def hasEntityQualifier(fq: String): Boolean = {
    val queryFuture = db.run(entitiesTable.filter(row => row.qualifier === fq).exists.result)
    Await.result(queryFuture, 10.seconds)
  }
}
