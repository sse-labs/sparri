package org.anon.spareuse.mvnem.storage.impl

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaPackage, JavaProgram}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaClasses, JavaFieldAccessRepr, JavaFieldAccessStatements, JavaInvocationRepr, JavaInvocationStatements, JavaMethodRepr, JavaMethods}
import org.anon.spareuse.core.storage.postgresql.{SoftwareEntities, SoftwareEntityRepr}
import org.anon.spareuse.core.utils.toHex
import org.anon.spareuse.mvnem.storage.EntityMinerStorageAdapter

import scala.util.{Failure, Success, Try}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class PostgresStorageAdapter(implicit executor: ExecutionContext) extends EntityMinerStorageAdapter {

  lazy val db = Database.forConfig("spa-reuse.postgres")
  val entitiesTable = TableQuery[SoftwareEntities]
  val javaClassesTable = TableQuery[JavaClasses]
  val javaMethodsTable = TableQuery[JavaMethods]
  val javaInvocationsTable = TableQuery[JavaInvocationStatements]
  val javaFieldAccessesTable = TableQuery[JavaFieldAccessStatements]

  lazy val idReturningEntitiesTable = entitiesTable returning entitiesTable.map(_.id)
  lazy val qualifierAndIdReturningEntitiesTable = entitiesTable returning entitiesTable.map(row => (row.qualifier, row.id))

  // Converts representations of binary hashes in the core model (byte-array) to database-representations (hex-strings)
  private implicit def toHexOpt(byteOpt: Option[Array[Byte]]): Option[String] = byteOpt.map(toHex)

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
    //IMPROVE: More batching for performance improvement
    val res = idReturningEntitiesTable +=
      SoftwareEntityRepr(0, data.name, data.uid, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)

    Await.result(db.run(res), 10.seconds)
  }

  private def getIdForQualifier(fq: String): Long = {
    val queryFuture = db.run(entitiesTable.filter( row => row.qualifier === fq).take(1).map(_.id).result)
    Await.result(queryFuture, 10.seconds).head
  }

  private def storeProgram(jp: JavaProgram, programDbId: Long): Future[_] = {
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

    var start = System.currentTimeMillis()
    val allClasses = jp.getChildren.map { case jc: JavaClass => toEntityRepr(jc, Some(packageDbId))}.toSeq
    val classLookup = Await.result(batchedEntityInsertWithMapReturn(allClasses, 500), 20.seconds)

    Await.ready(batchedClassInsert(jp.getChildren.map { case jc: JavaClass =>
      toClassRepr(jc, classLookup(jc.uid))}.toSeq, 500), 20.seconds)

    var duration = System.currentTimeMillis() - start

    log.debug(s"Stored all classes for package ${jp.uid} in $duration ms.")

    start = System.currentTimeMillis()
    val allMethods = jp.getChildren.flatMap( jc => jc.getChildren.map{ case jm: JavaMethod => toEntityRepr(jm, Some(classLookup(jc.uid)))}).toSeq
    val methodLookup = Await.result(batchedEntityInsertWithMapReturn(allMethods, 500), 30.seconds)

    Await.ready(batchedMethodInsert(jp.getChildren.flatMap(_.getChildren).map { case jm: JavaMethod =>
      toMethodRepr(jm, methodLookup(jm.uid))}.toSeq, 500), 30.seconds)

    duration = System.currentTimeMillis() - start

    log.debug(s"Stored all methods for package ${jp.uid} in $duration ms.")

    start = System.currentTimeMillis()
    val allInstructions = jp.getChildren.flatMap(jc => jc.getChildren).flatMap(jm => jm.getChildren.map {
      case jis: JavaInvokeStatement =>
        toEntityRepr(jis, Some(methodLookup(jm.uid)))
      case jfas: JavaFieldAccessStatement =>
        toEntityRepr(jfas, Some(methodLookup(jm.uid)))
    }).toSeq

    val statementLookup = Await.result(batchedEntityInsertWithMapReturn(allInstructions, 500), 40.seconds)

    val allInstructionsRaw = jp.getChildren.flatMap(_.getChildren).flatMap(_.getChildren)

    val allInvocations = allInstructionsRaw.filter(_.isInstanceOf[JavaInvokeStatement]).map { case jis: JavaInvokeStatement => toInvocationRepr(jis, statementLookup(jis.uid))}.toSeq
    val allFieldAccesses = allInstructionsRaw.filter(_.isInstanceOf[JavaFieldAccessStatement]).map { case jfas: JavaFieldAccessStatement => toFieldAccessRepr(jfas, statementLookup(jfas.uid))}.toSeq

    val result = Future.sequence(Seq(batchedInvocationInsert(allInvocations, 500), batchedFieldAccessInsert(allFieldAccesses, 500)))

    duration = System.currentTimeMillis() - start

    log.debug(s"Stored all instructions for package ${jp.uid} in $duration ms.")

    result.map( _ => () )
  }

  private def toEntityRepr(data: SoftwareEntityData, parentIdOpt: Option[Long]): SoftwareEntityRepr = SoftwareEntityRepr(0, data.name,
    data.uid, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)

  private def toClassRepr(jc: JavaClass, parentId: Long): JavaClassRepr = (parentId, jc.thisType, jc.superType, jc.interfaceTypes.mkString(";"), jc.isInterface, jc.isFinal, jc.isAbstract)

  private def toMethodRepr(jm: JavaMethod, parentId: Long): JavaMethodRepr = (parentId, jm.returnType, jm.paramCount, jm.paramTypes.mkString(","), jm.isFinal, jm.isStatic, jm.isAbstract, jm.visibility, jm.methodHash)

  private def toInvocationRepr(jis: JavaInvokeStatement, parentId: Long): JavaInvocationRepr =
    (parentId, jis.targetTypeName, jis.targetMethodParameterCount, jis.returnTypeName, jis.invokeStatementType.id, jis.instructionPc)

  private def toFieldAccessRepr(jfas: JavaFieldAccessStatement, parentId: Long): JavaFieldAccessRepr =
    (parentId, jfas.targetFieldTypeName, jfas.targetTypeName, jfas.fieldAccessType.id, jfas.instructionPc)

  private def batchedEntityInsertWithMapReturn(entityReprs: Seq[SoftwareEntityRepr], batchSize: Int): Future[Map[String, Long]] = {

    Future.sequence(
      entityReprs.grouped(batchSize).map { batch => db.run(qualifierAndIdReturningEntitiesTable ++= batch).map( resultObj => resultObj.toMap)}.toSeq)
      .map(seqOfMaps => seqOfMaps.flatten.toMap)
  }

  private def batchedEntityInsert(entityReprs: Seq[SoftwareEntityRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(entityReprs.grouped(batchSize).map { batch => db.run(entitiesTable ++= batch)}.toSeq).map( _ => ())
  }

  private def batchedClassInsert(data: Seq[JavaClassRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaClassesTable ++= batch)}.toSeq).map( _ => ())
  }

  private def batchedMethodInsert(data: Seq[JavaMethodRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaMethodsTable ++= batch)}.toSeq).map( _ => ())
  }

  private def batchedInvocationInsert(data: Seq[JavaInvocationRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaInvocationsTable ++= batch)}.toSeq).map( _ => ())
  }

  private def batchedFieldAccessInsert(data: Seq[JavaFieldAccessRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaFieldAccessesTable ++= batch)}.toSeq).map( _ => ())
  }




  override def initialize(): Unit = {
    val setupAction = DBIO.seq(entitiesTable.schema.createIfNotExists, javaClassesTable.schema.createIfNotExists,
      javaMethodsTable.schema.createIfNotExists, javaInvocationsTable.schema.createIfNotExists, javaFieldAccessesTable.schema.createIfNotExists)

    val setupFuture = db.run(setupAction)

    Await.ready(setupFuture, 20.seconds)
  }

  override def shutdown(): Unit = db.close()

  override def hasEntityQualifier(fq: String): Boolean = {
    val queryFuture = db.run(entitiesTable.filter(row => row.qualifier === fq).exists.result)
    Await.result(queryFuture, 10.seconds)
  }
}
