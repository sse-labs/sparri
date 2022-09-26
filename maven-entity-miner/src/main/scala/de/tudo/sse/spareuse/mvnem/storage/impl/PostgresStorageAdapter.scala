package de.tudo.sse.spareuse.mvnem.storage.impl

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaPackage, JavaProgram}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.storage.postgresql.{SoftwareEntities, SoftwareEntityRepr}
import de.tudo.sse.spareuse.mvnem.storage.EntityMinerStorageAdapter

import scala.util.{Failure, Success, Try}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

class PostgresStorageAdapter(implicit executor: ExecutionContext) extends EntityMinerStorageAdapter {

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
    log.debug(s"Storing java program ${data.name}")
    storeProgram(data, currentId)
  }

  private def storeDataAndGetId(data: SoftwareEntityData, parentIdOpt: Option[Long]): Long = {
    //TODO: Maybe batching?
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
    }.toSeq

    val insertAction = batchedEntityInsertWithMapReturn(batch, 100)

    val insertResult = Await.result(insertAction, 10.seconds)

    val classLookup = jp.getChildren.map(m => (m.uid, m.asInstanceOf[JavaPackage])).toMap

    Future.sequence(insertResult.map( tuple => storePackage(classLookup(tuple._1), tuple._2)).toSeq)
  }

  private def storePackage(jp: JavaPackage, packageDbId: Long): Future[Unit] = {
    //TODO: Specialization tables
    var start = System.currentTimeMillis()
    val allClasses = jp.getChildren.map { case jc: JavaClass => toEntityRepr(jc, Some(packageDbId))}.toSeq
    val classLookup = Await.result(batchedEntityInsertWithMapReturn(allClasses, 500), 20.seconds)
    var duration = System.currentTimeMillis() - start

    log.debug(s"Stored all classes for package ${jp.uid} in $duration ms.")

    start = System.currentTimeMillis()
    val allMethods = jp.getChildren.flatMap( jc => jc.getChildren.map{ case jm: JavaMethod => toEntityRepr(jm, Some(classLookup(jc.uid)))}).toSeq
    val methodLookup = Await.result(batchedEntityInsertWithMapReturn(allMethods, 500), 30.seconds)
    duration = System.currentTimeMillis() - start

    log.debug(s"Stored all methods for package ${jp.uid} in $duration ms.")

    start = System.currentTimeMillis()
    val allInstructions = jp.getChildren.flatMap(jc => jc.getChildren).flatMap(jm => jm.getChildren.map {
      case jis: JavaInvokeStatement =>
        toEntityRepr(jis, Some(methodLookup(jm.uid)))
      case jfas: JavaFieldAccessStatement =>
        toEntityRepr(jfas, Some(methodLookup(jm.uid)))
    }).toSeq

    val result = batchedEntityInsert(allInstructions, 500)
    duration = System.currentTimeMillis() - start

    log.debug(s"Stored all instructions for package ${jp.uid} in $duration ms.")

    result
  }

  private def toEntityRepr(data: SoftwareEntityData, parentIdOpt: Option[Long]): SoftwareEntityRepr = (0, data.name,
    data.uid, data.language, data.kind.id, data.repository, parentIdOpt)

  private def batchedEntityInsertWithMapReturn(entityReprs: Seq[SoftwareEntityRepr], batchSize: Int): Future[Map[String, Long]] = {

    Future.sequence(
      entityReprs.grouped(batchSize).map { batch => db.run(qualifierAndIdReturningEntitiesTable ++= batch).map( resultObj => resultObj.toMap)}.toSeq)
      .map(seqOfMaps => seqOfMaps.flatten.toMap)
  }

  private def batchedEntityInsert(entityReprs: Seq[SoftwareEntityRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(entityReprs.grouped(batchSize).map { batch => db.run(entitiesTable ++= batch)}.toSeq).map( _ => ())
  }




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
