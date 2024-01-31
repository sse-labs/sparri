package org.anon.spareuse.mvnem.storage.impl

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaNewInstanceStatement, JavaPackage, JavaProgram, JavaStatement}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaClasses, JavaFieldAccessRepr, JavaFieldAccessStatements, JavaInvocationRepr, JavaInvocationStatements, JavaMethodRepr, JavaMethods}
import org.anon.spareuse.core.storage.postgresql.{SoftwareEntities, SoftwareEntityRepr}
import org.anon.spareuse.core.utils.toHex
import org.anon.spareuse.mvnem.storage.EntityMinerStorageAdapter

import scala.util.Try
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


  override def ensureNotPresent(programUid: String): Unit = {
    if(hasEntityQualifier(programUid)){

      Try{
        val programId = getIdForQualifier(programUid)
        val packageIds = Await.result(db.run(entitiesTable.filter(e => e.parentID === programId).map(_.id).result), 30.seconds).toSet
        val classIds = Await.result(db.run(entitiesTable.filter(e => e.parentID inSet packageIds).map(_.id).result), 60.seconds).toSet
        val methodIds = Await.result(db.run(entitiesTable.filter(e => e.parentID inSet classIds).map(_.id).result), 80.seconds).toSet
        val statementIds = Await.result(db.run(entitiesTable.filter(e => e.parentID inSet methodIds).map(_.id).result), 120.seconds).toSet

        val deleteActions = DBIO.sequence(Seq(
          javaInvocationsTable.filter(i => i.id inSet statementIds).delete,
          javaFieldAccessesTable.filter(f => f.id inSet statementIds).delete,
          javaMethodsTable.filter(m => m.id inSet methodIds).delete,
          javaClassesTable.filter(c => c.id inSet classIds).delete,
          entitiesTable.filter(e => e.id inSet statementIds).delete,
          entitiesTable.filter(e => e.id inSet methodIds).delete,
          entitiesTable.filter(e => e.id inSet classIds).delete,
          entitiesTable.filter(e => e.id inSet packageIds).delete,
          entitiesTable.filter(e => e.id === programId).delete)
        )

        Await.ready(db.run(deleteActions), 300.seconds)
      }
    }
  }

  override def storeJavaProgram(jp: JavaProgram): Try[Unit] = Try {

    // Create parent if it does not already exist, get uid of parent entity
    val parentIdOpt = jp.getParent.map(p => {
      if (hasEntityQualifier(p.uid)) {
        getIdForQualifier(p.uid)
      } else {
        storeDataAndGetId(p, None)
      }
    })

    // Programs don't have a separate additional table, so they only need this one insert
    val programId = storeDataAndGetId(jp, parentIdOpt)

    def storeEntitiesTimed(entities: Set[SoftwareEntityData], parentIdLookup: Map[String, Long], batchSize: Int, kindName: String): Map[String, Long] = {
      val timeout = Math.max((entities.size / 100) * 10, 30).seconds // 10 seconds for every 100 entities, at least 30 seconds
      val start = System.currentTimeMillis()
      val result = Await.result(insertEntitiesBatchedWithMapReturn(entities, parentIdLookup, batchSize), timeout)
      val time = System.currentTimeMillis() - start
      log.debug(s"Successfully stored ${entities.size} entities of kind $kindName for program ${jp.name} in $time ms.")
      result
    }

    val packageLookup = storeEntitiesTimed(jp.getChildren, Map(jp.uid -> programId), 30, "Package")
    val allClasses = jp.getChildren.flatMap(_.getChildren)
    val classLookup = storeEntitiesTimed(allClasses, packageLookup, 50, "Class")
    val allMethods = allClasses.flatMap(_.getChildren)
    val methodLookup = storeEntitiesTimed(allMethods, classLookup, 50, "Methods")
    val allStatements = allMethods.flatMap(_.getChildren)
    storeEntitiesTimed(allStatements, methodLookup, 100, "Statements")

  }

  private[storage] def insertEntitiesBatchedWithMapReturn(entities: Set[SoftwareEntityData], parentIdLookup: Map[String, Long], batchSize: Int): Future[Map[String, Long]] = {
    val entityReprs = entities.map(e => toEntityRepr(e, parentIdLookup.get(e.uid)))

    batchedEntityInsertWithMapReturn(entityReprs, batchSize).flatMap { theMap =>
      entities.headOption match {
        case Some(_: JavaClass) =>
          val allClasses = entities.filter(_.isInstanceOf[JavaClass]).map{ case jc: JavaClass => toClassRepr(jc, theMap(jc.uid)) }.toSeq
          batchedClassInsert(allClasses, batchSize).map(_ => theMap)
        case Some(_: JavaMethod) =>
          val allMethods = entities.filter(_.isInstanceOf[JavaMethod]).map{ case jm: JavaMethod => toMethodRepr(jm, theMap(jm.uid)) }.toSeq
          batchedMethodInsert(allMethods, batchSize).map(_ => theMap)
        case Some(_: JavaStatement) =>
          val allInvocations = entities.filter(_.isInstanceOf[JavaInvokeStatement]).map { case jis: JavaInvokeStatement => toInvocationRepr(jis, theMap(jis.uid)) }.toSeq
          val allFieldAccesses = entities.filter(_.isInstanceOf[JavaFieldAccessStatement]).map { case jfas: JavaFieldAccessStatement => toFieldAccessRepr(jfas, theMap(jfas.uid)) }.toSeq
          batchedInvocationInsert(allInvocations, batchSize).andThen(_ => batchedFieldAccessInsert(allFieldAccesses, batchSize)).map(_ => theMap)
        case _ =>
          // Do nothing if there are no entities to insert or the entity kind does not need an extra table insert
          Future.successful(Map.empty[String, Long])
      }
    }
  }

  private def storeDataAndGetId(data: SoftwareEntityData, parentIdOpt: Option[Long]): Long = {
    val res = idReturningEntitiesTable +=
      SoftwareEntityRepr(0, data.name, data.uid, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)
    Await.result(db.run(res), 10.seconds)
  }

  private def getIdForQualifier(fq: String): Long = {
    val queryFuture = db.run(entitiesTable.filter( row => row.qualifier === fq).take(1).map(_.id).result)
    Await.result(queryFuture, 10.seconds).head
  }

  private def toEntityRepr(data: SoftwareEntityData, parentIdOpt: Option[Long]): SoftwareEntityRepr = SoftwareEntityRepr(0, data.name,
    data.uid, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)

  private def toClassRepr(jc: JavaClass, parentId: Long): JavaClassRepr = (parentId, jc.thisType, jc.superType, jc.interfaceTypes.mkString(";"), jc.isInterface, jc.isFinal, jc.isAbstract)

  private def toMethodRepr(jm: JavaMethod, parentId: Long): JavaMethodRepr = (parentId, jm.descriptor, jm.isFinal, jm.isStatic, jm.isAbstract, jm.visibility, jm.methodHash)

  private def toInvocationRepr(jis: JavaInvokeStatement, parentId: Long): JavaInvocationRepr =
    (parentId, jis.targetTypeName, jis.targetDescriptor, jis.invokeStatementType.id, jis.instructionPc)

  private def toFieldAccessRepr(jfas: JavaFieldAccessStatement, parentId: Long): JavaFieldAccessRepr =
    (parentId, jfas.targetFieldTypeName, jfas.targetTypeName, jfas.fieldAccessType.id, jfas.instructionPc)

  private def batchedEntityInsertWithMapReturn(entityReprs: Iterable[SoftwareEntityRepr], batchSize: Int): Future[Map[String, Long]] = {

    Future
      .sequence(
        entityReprs
          .grouped(batchSize)
          .map { batch => db.run(qualifierAndIdReturningEntitiesTable ++= batch).map( resultObj => resultObj.toMap)}
      )
      .map(seqOfMaps => seqOfMaps.flatten.toMap)
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

    val s = db.createSession()
    val autoCommit = s.conn.getAutoCommit
    s.close()

    log.info(s"Database setup complete, auto-commit value: $autoCommit")
  }

  override def shutdown(): Unit = db.close()

  override def hasEntityQualifier(fq: String): Boolean = {
    val queryFuture = db.run(entitiesTable.filter(row => row.qualifier === fq).exists.result)
    Await.result(queryFuture, 10.seconds)
  }
}
