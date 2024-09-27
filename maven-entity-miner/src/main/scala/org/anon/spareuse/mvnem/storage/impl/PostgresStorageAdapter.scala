package org.anon.spareuse.mvnem.storage.impl

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaProgram, JavaStatement}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaClasses, JavaFieldAccessRepr, JavaFieldAccessStatements, JavaInvocationRepr, JavaInvocationStatements, JavaMethodRepr, JavaMethods, JavaPrograms}
import org.anon.spareuse.core.storage.postgresql.{SoftwareEntities, SoftwareEntityRepr}
import org.anon.spareuse.core.utils.toHex
import org.anon.spareuse.mvnem.storage.EntityMinerStorageAdapter
import slick.dbio.DBIO

import scala.util.{Success, Try}
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class PostgresStorageAdapter(implicit executor: ExecutionContext) extends EntityMinerStorageAdapter {

  lazy val db = Database.forConfig("spa-reuse.postgres")
  val entitiesTable = TableQuery[SoftwareEntities]
  val javaProgramsTable = TableQuery[JavaPrograms]
  val javaClassesTable = TableQuery[JavaClasses]
  val javaMethodsTable = TableQuery[JavaMethods]
  val javaInvocationsTable = TableQuery[JavaInvocationStatements]
  val javaFieldAccessesTable = TableQuery[JavaFieldAccessStatements]

  lazy val idReturningEntitiesTable = entitiesTable returning entitiesTable.map(_.id)
  lazy val qualifierAndIdReturningEntitiesTable = entitiesTable returning entitiesTable.map(row => (row.qualifier, row.id))

  // Converts representations of binary hashes in the core model (byte-array) to database-representations (hex-strings)
  private[mvnem] implicit def toHexOpt(byteOpt: Option[Array[Byte]]): Option[String] = byteOpt.map(toHex)


  override def ensureNotPresent(programUid: String): Unit = {

    def getIdForQualifier(fq: String): Long = {
      val queryFuture = db.run(entitiesTable.filter(row => row.qualifier === fq).take(1).map(_.id).result)
      Await.result(queryFuture, 10.seconds).head
    }

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
          javaProgramsTable.filter(p => p.id === programId).delete,
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

  override def storeJavaProgram(jp: JavaProgram): Future[String] = {

    def idForQualifierF(fq: String): Future[Option[Long]] = {
      db.run(entitiesTable.filter(row => row.qualifier === fq).take(1).map(_.id).result).map(_.headOption)
    }

    // Query for parent, return id if present, create entry and return id otherwise
    val parentIdOptF = jp.getParent.map { parent =>
      idForQualifierF(parent.uid).flatMap {
        case Some(id) =>
          Future(id) // If the parent (library) is already present, we return its id
        case None =>
          storeDataAndGetId(parent, None) // If not, we try to create it and return its id
            .recoverWith(_ => idForQualifierF(parent.uid).map(_.get)) // If that fails, it is likely because another thread created the library -> Re-try getting its id
      }
    }

    // Chain creation of actual program entity and its custom table entry to parent id future
    val programIdF = (if (parentIdOptF.isDefined) parentIdOptF.get.flatMap(pId => storeDataAndGetId(jp, Some(pId))) else storeDataAndGetId(jp, None))
      .flatMap { pId =>
        db.run(javaProgramsTable += (pId, jp.publishedAt)).map(_ => pId)
      }

    val allPackages = jp.getChildren
    val allClasses = jp.getChildren.flatMap(_.getChildren)
    val allMethods = allClasses.flatMap(_.getChildren)
    val allStatements = allMethods.flatMap(_.getChildren)

    def batchSizeFor(set: Set[SoftwareEntityData], suggestedSize: Int): Int = {
      var currSize = suggestedSize
      while (set.size / currSize > 300) {
        currSize = currSize * 15 / 10
      }
      currSize
    }

    val insertionFuture = programIdF
      .flatMap { pId =>
        insertEntitiesBatchedWithMapReturn(allPackages, Map(jp.uid -> pId), batchSizeFor(allPackages, 30))
      }
      .flatMap { pIds =>
        insertEntitiesBatchedWithMapReturn(allClasses, pIds, batchSizeFor(allClasses, 50))
          .andThen {
            case Success(_) =>
              log.debug(s"Successfully stored ${allClasses.size} classes for program ${jp.name}.")
          }
      }
      .flatMap { pIds =>
        insertEntitiesBatchedWithMapReturn(allMethods, pIds, batchSizeFor(allMethods, 50))
          .andThen {
            case Success(_) =>
              log.debug(s"Successfully stored ${allMethods.size} methods for program ${jp.name}.")
          }
      }
      .flatMap { pIds =>
        insertEntitiesBatchedWithMapReturn(allStatements, pIds, batchSizeFor(allStatements, 200))
          .andThen {
            case Success(_) =>
              log.debug(s"Successfully stored ${allStatements.size} statements for program ${jp.name}.")
          }
      }
    insertionFuture.map(_ => jp.programName)
  }

  private[storage] def insertEntitiesBatchedWithMapReturn(entities: Set[SoftwareEntityData], parentIdLookup: Map[String, Long], batchSize: Int): Future[Map[String, Long]] = {
    val entityReprs = entities.map(e => toEntityRepr(e, e.getParent.flatMap(eP => parentIdLookup.get(eP.uid))))

    batchedEntityInsertWithMapReturn(entityReprs, batchSize).flatMap { theMap =>
      entities.headOption match {
        case Some(_: JavaClass) =>
          val allClasses = entities.collect { case jc: JavaClass => toClassRepr(jc, theMap(jc.uid)) }.toSeq
          batchedClassInsert(allClasses, batchSize).map(_ => theMap)
        case Some(_: JavaMethod) =>
          val allMethods = entities.collect { case jm: JavaMethod => toMethodRepr(jm, theMap(jm.uid)) }.toSeq
          batchedMethodInsert(allMethods, batchSize).map(_ => theMap)
        case Some(_: JavaStatement) =>
          val allInvocations = entities.collect { case jis: JavaInvokeStatement => toInvocationRepr(jis, theMap(jis.uid)) }.toSeq
          val allFieldAccesses = entities.collect { case jfas: JavaFieldAccessStatement => toFieldAccessRepr(jfas, theMap(jfas.uid)) }.toSeq
          batchedInvocationInsert(allInvocations, batchSize).flatMap(_ => batchedFieldAccessInsert(allFieldAccesses, batchSize)).map(_ => theMap)
        case _ =>
          // Do nothing if there are no entities to insert or the entity kind does not need an extra table insert
          Future(theMap)
      }
    }
  }

  private[storage] def storeDataAndGetId(data: SoftwareEntityData, parentIdOpt: Option[Long]): Future[Long] = {
    val res = idReturningEntitiesTable +=
      SoftwareEntityRepr(0, data.name, data.uid, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)
    db.run(res)
  }

  private[storage] def batchedEntityInsertWithMapReturn(entityReprs: Iterable[SoftwareEntityRepr], batchSize: Int): Future[Map[String, Long]] = {

    Future
      .sequence(
        entityReprs
          .grouped(batchSize)
          .map { batch => db.run(qualifierAndIdReturningEntitiesTable ++= batch).map(resultObj => resultObj.toMap) }
      )
      .map(seqOfMaps => seqOfMaps.flatten.toMap)
  }

  private def batchedClassInsert(data: Seq[JavaClassRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaClassesTable ++= batch) }.toSeq).map(_ => ())
  }

  private def batchedMethodInsert(data: Seq[JavaMethodRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaMethodsTable ++= batch) }.toSeq).map(_ => ())
  }

  private def batchedInvocationInsert(data: Seq[JavaInvocationRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaInvocationsTable ++= batch) }.toSeq).map(_ => ())
  }

  private def batchedFieldAccessInsert(data: Seq[JavaFieldAccessRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaFieldAccessesTable ++= batch) }.toSeq).map(_ => ())
  }



  private def toEntityRepr(data: SoftwareEntityData, parentIdOpt: Option[Long]): SoftwareEntityRepr = SoftwareEntityRepr(0, data.name,
    data.uid, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)

  private def toClassRepr(jc: JavaClass, parentId: Long): JavaClassRepr = (parentId, jc.thisType, jc.superType, jc.interfaceTypes.mkString(";"), jc.isInterface, jc.isFinal, jc.isAbstract)

  private def toMethodRepr(jm: JavaMethod, parentId: Long): JavaMethodRepr = (parentId, jm.descriptor, jm.isFinal, jm.isStatic, jm.isAbstract, jm.visibility, jm.methodHash)

  private def toInvocationRepr(jis: JavaInvokeStatement, parentId: Long): JavaInvocationRepr =
    (parentId, jis.targetTypeName, jis.targetDescriptor, jis.invokeStatementType.id, jis.instructionPc)

  private def toFieldAccessRepr(jfas: JavaFieldAccessStatement, parentId: Long): JavaFieldAccessRepr =
    (parentId, jfas.targetFieldTypeName, jfas.targetTypeName, jfas.fieldAccessType.id, jfas.instructionPc)

  override def initialize(): Unit = {
    val allEntityTables = Seq(entitiesTable, javaProgramsTable, javaClassesTable, javaMethodsTable, javaInvocationsTable,
      javaFieldAccessesTable)

    val existing = db.run(MTable.getTables)
    val f = existing.flatMap(v => {
      val names = v.map(mt => mt.name.name)
      val createIfNotExist = allEntityTables.filter(table => !names.contains(table.baseTableRow.tableName)).map(_.schema.create)
      db.run(DBIO.sequence(createIfNotExist))
    })(executor)

    Await.ready(f, 60.seconds)

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
