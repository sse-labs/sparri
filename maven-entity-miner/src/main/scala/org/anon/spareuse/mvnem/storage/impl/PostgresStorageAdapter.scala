package org.anon.spareuse.mvnem.storage.impl

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaProgram, JavaStatement, PathIdentifiableJavaEntity}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassInterface, JavaClassInterfaces, JavaClassRepr, JavaClasses, JavaFieldAccessRepr, JavaFieldAccessStatements, JavaInvocationRepr, JavaInvocationStatements, JavaMethodDescriptor, JavaMethodDescriptors, JavaMethodRepr, JavaMethods, JavaPrograms, JavaTypeName, JavaTypeNames}
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
  val typeNameTable = TableQuery[JavaTypeNames]
  val descriptorTable = TableQuery[JavaMethodDescriptors]
  val javaProgramsTable = TableQuery[JavaPrograms]
  val javaClassesTable = TableQuery[JavaClasses]
  val javaClassInterfacesTable = TableQuery[JavaClassInterfaces]
  val javaMethodsTable = TableQuery[JavaMethods]
  val javaInvocationsTable = TableQuery[JavaInvocationStatements]
  val javaFieldAccessesTable = TableQuery[JavaFieldAccessStatements]

  lazy val idReturningEntitiesTable = entitiesTable returning entitiesTable.map(_.id)
  lazy val idReturningTypeNameTable = typeNameTable returning typeNameTable.map(_.id)
  lazy val idReturningDescriptorTable = descriptorTable returning descriptorTable.map(_.id)
  lazy val identifierAndParentAndIdReturningEntitiesTable = entitiesTable returning entitiesTable.map(row => (row.identifier, row.parentID, row.id))

  // Converts representations of binary hashes in the core model (byte-array) to database-representations (hex-strings)
  private[mvnem] implicit def toHexOpt(byteOpt: Option[Array[Byte]]): Option[String] = byteOpt.map(toHex)


  override def ensureProgramNotPresent(programGav: String): Unit = {

    def getProgramEntityId(gav: String): Option[Long] = {
      val queryF = db.run(entitiesTable.filter(e => e.kind === SoftwareEntityKind.Program.id).filter(e => e.name === gav).take(1).map(_.id).result)
      Await.result(queryF, 10.seconds).headOption
    }

    val programIdOpt = getProgramEntityId(programGav)

    if(programIdOpt.isDefined){

      Try{
        val programId = programIdOpt.get
        val packageIds = Await.result(db.run(entitiesTable.filter(e => e.parentID === programId).map(_.id).result), 30.seconds).toSet
        val classIds = Await.result(db.run(entitiesTable.filter(e => e.parentID inSet packageIds).map(_.id).result), 60.seconds).toSet
        val methodIds = Await.result(db.run(entitiesTable.filter(e => e.parentID inSet classIds).map(_.id).result), 80.seconds).toSet
        val statementIds = Await.result(db.run(entitiesTable.filter(e => e.parentID inSet methodIds).map(_.id).result), 120.seconds).toSet


        val deleteActions = DBIO.sequence(Seq(
          javaInvocationsTable.filter(i => i.id inSet statementIds).delete,
          javaFieldAccessesTable.filter(f => f.id inSet statementIds).delete,
          javaMethodsTable.filter(m => m.id inSet methodIds).delete,
          javaClassInterfacesTable.filter(i => i.classId inSet classIds).delete,
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

    def libraryIdF(ga: String): Future[Option[Long]] = {
      db.run(entitiesTable.filter(e => e.parentID.isEmpty &&  e.identifier === ga).take(1).map(_.id).result).map(_.headOption)
    }

    // Query for parent, return id if present, create entry and return id otherwise
    val parentIdOptF = jp.getParent.map { parent =>
      val libraryGA = parent.name
      libraryIdF(libraryGA).flatMap {
        case Some(id) =>
          Future(id) // If the parent (library) is already present, we return its id
        case None =>
          storeDataAndGetId(parent, None) // If not, we try to create it and return its id
            .recoverWith(_ => libraryIdF(libraryGA).map(_.get)) // If that fails, it is likely because another thread created the library -> Re-try getting its id
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

    val allTypeNames = allClasses.flatMap {
      case jc: JavaClass =>
        Set(jc.thisType) ++ jc.superType.toSet ++ jc.interfaceTypes
    } ++ allStatements.collect {
      case invoke: JavaInvokeStatement =>
        Set(invoke.targetTypeName)
      case field: JavaFieldAccessStatement =>
        Set(field.targetFieldTypeName, field.targetTypeName)
    }.flatten

    val allNamesFuture = Future.sequence(allTypeNames.map{ name =>
      db.run(typeNameTable.filter(_.name === name).take(1).map(_.id).result).flatMap{ idSeq =>
        if(idSeq.isEmpty){
          db
            .run(idReturningTypeNameTable += JavaTypeName(0, name))
            .map(idResult => (name, idResult))
        } else {
          Future.successful((name, idSeq.head))
        }
      }
    })

    val typeNameLookup = Await.result(allNamesFuture, 60.seconds).toMap

    log.debug(s"Successfully indexed ${typeNameLookup.size} type names for program ${jp.name}")


    def insertDescriptorBatchF(batch: Set[String]): Future[Set[(String, Long)]] = {
      Future.sequence(batch.map { descriptor =>
        db.run(descriptorTable.filter(_.descriptor === descriptor).take(1).map(_.id).result).flatMap { idSeq =>
          if (idSeq.isEmpty) {
            db
              .run(idReturningDescriptorTable += JavaMethodDescriptor(0, descriptor))
              .map(idResult => (descriptor, idResult))
          } else {
            Future.successful((descriptor, idSeq.head))
          }
        }
      })
    }

    val allDescriptorBatches = (allMethods.map { case jm: JavaMethod => jm.descriptor } ++
      allStatements.collect { case invoke: JavaInvokeStatement => invoke.targetDescriptor }).grouped(50).toSeq

    val descriptorLookup = if(allDescriptorBatches.nonEmpty){
      var allDescriptorsFuture: Future[Set[(String, Long)]] = null

      allDescriptorBatches.foreach{ batch =>
        if(allDescriptorsFuture == null)
          allDescriptorsFuture = insertDescriptorBatchF(batch)
        else
          allDescriptorsFuture = allDescriptorsFuture.flatMap { results =>
            insertDescriptorBatchF(batch).map(newResult => newResult ++ results)
          }
      }

      Await.result(allDescriptorsFuture, 60.seconds).toMap
    } else Map.empty[String, Long]



    log.debug(s"Successfully indexed ${descriptorLookup.size} descriptors for program ${jp.name}")

    def batchSizeFor(set: Set[SoftwareEntityData], suggestedSize: Int): Int = {
      var currSize = suggestedSize
      while (set.size / currSize > 300) {
        currSize = currSize * 15 / 10
      }
      currSize
    }

    val insertionFuture = programIdF
      .flatMap { pId =>
        insertEntitiesBatchedWithMapReturn(allPackages, Map(jp.uid -> pId), batchSizeFor(allPackages, 30),
          typeNameLookup, descriptorLookup)
      }
      .flatMap { pIds =>
        insertEntitiesBatchedWithMapReturn(allClasses, pIds, batchSizeFor(allClasses, 50),
          typeNameLookup, descriptorLookup)
          .andThen {
            case Success(_) =>
              log.debug(s"Successfully stored ${allClasses.size} classes for program ${jp.name}.")
          }
      }
      .flatMap { pIds =>
        insertEntitiesBatchedWithMapReturn(allMethods, pIds, batchSizeFor(allMethods, 50),
          typeNameLookup, descriptorLookup)
          .andThen {
            case Success(_) =>
              log.debug(s"Successfully stored ${allMethods.size} methods for program ${jp.name}.")
          }
      }
      .flatMap { pIds =>
        insertEntitiesBatchedWithMapReturn(allStatements, pIds, batchSizeFor(allStatements, 200),
          typeNameLookup, descriptorLookup)
          .andThen {
            case Success(_) =>
              log.debug(s"Successfully stored ${allStatements.size} statements for program ${jp.name}.")
          }
      }
    insertionFuture.map(_ => jp.programName)
  }

  private[storage] def insertEntitiesBatchedWithMapReturn(entities: Set[SoftwareEntityData],
                                                          parentIdLookup: Map[String, Long],
                                                          batchSize: Int,
                                                          typeNameLookup: Map[String, Long],
                                                          descriptorLookup: Map[String, Long]): Future[Map[String, Long]] = {
    val entityReprs = entities.map(e => (e.asInstanceOf[PathIdentifiableJavaEntity].uid, toEntityRepr(e, e.getParent.flatMap(eP => parentIdLookup.get(eP.asInstanceOf[PathIdentifiableJavaEntity].uid)))))

    batchedEntityInsertWithMapReturn(entityReprs, batchSize).flatMap { theMap =>
      entities.headOption match {
        case Some(_: JavaClass) =>
          val allClasses = entities.collect { case jc: JavaClass => toClassRepr(jc, theMap(jc.uid), typeNameLookup) }.toSeq
          batchedClassInsert(allClasses, batchSize)
            .flatMap{ _ =>
              val data = entities.collect { case jc: JavaClass => jc.interfaceTypes.map(typeNameLookup).map(iid => JavaClassInterface(0, theMap(jc.uid), iid))}.flatten
              batchedClassInterfaceInsert(data, batchSize)
            }
            .map(_ => theMap)
        case Some(_: JavaMethod) =>
          val allMethods = entities.collect { case jm: JavaMethod => toMethodRepr(jm, theMap(jm.uid), descriptorLookup) }.toSeq
          batchedMethodInsert(allMethods, batchSize).map(_ => theMap)
        case Some(_: JavaStatement) =>
          val allInvocations = entities.collect { case jis: JavaInvokeStatement =>
            toInvocationRepr(jis, theMap(jis.uid), typeNameLookup, descriptorLookup) }.toSeq
          val allFieldAccesses = entities.collect { case jfas: JavaFieldAccessStatement =>
            toFieldAccessRepr(jfas, theMap(jfas.uid), typeNameLookup) }.toSeq
          batchedInvocationInsert(allInvocations, batchSize).flatMap(_ => batchedFieldAccessInsert(allFieldAccesses, batchSize)).map(_ => theMap)
        case _ =>
          // Do nothing if there are no entities to insert or the entity kind does not need an extra table insert
          Future(theMap)
      }
    }
  }

  private[storage] def storeDataAndGetId(data: SoftwareEntityData, parentIdOpt: Option[Long]): Future[Long] = {
    val res = idReturningEntitiesTable +=
      SoftwareEntityRepr(0, data.name, data.identifier, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)
    db.run(res)
  }

  private[storage] def batchedEntityInsertWithMapReturn(entityReprs: Iterable[(String, SoftwareEntityRepr)], batchSize: Int): Future[Map[String, Long]] = {

    val uidLookup = entityReprs.map(t => ((t._2.parentId, t._2.identifier), t._1)).toMap

    Future
      .sequence(
        entityReprs
          .grouped(batchSize)
          .map { batch => db.run(identifierAndParentAndIdReturningEntitiesTable ++= batch.map(_._2)).map(resultObj => resultObj.map(triple => (uidLookup((triple._2, triple._1)), triple._3)).toMap) }
      )
      .map(seqOfMaps => seqOfMaps.flatten.toMap)
  }

  private def batchedClassInsert(data: Seq[JavaClassRepr], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaClassesTable ++= batch) }.toSeq).map(_ => ())
  }

  private def batchedClassInterfaceInsert(data: Set[JavaClassInterface], batchSize: Int): Future[Unit] = {
    Future.sequence(data.grouped(batchSize).map { batch => db.run(javaClassInterfacesTable ++= batch) }.toSeq).map(_ => ())
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
    data.identifier, data.language, data.kind.id, data.repository, parentIdOpt, data.binaryHash)

  private def toClassRepr(jc: JavaClass, parentId: Long, typeNameLookup: Map[String, Long]): JavaClassRepr = {
    JavaClassRepr(parentId, typeNameLookup(jc.thisType), jc.superType.map(typeNameLookup), jc.isInterface, jc.isFinal, jc.isAbstract)
  }

  private def toMethodRepr(jm: JavaMethod, parentId: Long, descriptorLookup: Map[String, Long]): JavaMethodRepr =
    JavaMethodRepr(parentId, descriptorLookup(jm.descriptor), jm.isFinal, jm.isStatic, jm.isAbstract,
      jm.visibility, jm.methodHash)

  private def toInvocationRepr(jis: JavaInvokeStatement, parentId: Long, typeNameLookup: Map[String, Long],
                               descriptorLookup: Map[String, Long]): JavaInvocationRepr =
    JavaInvocationRepr(parentId, typeNameLookup(jis.targetTypeName), descriptorLookup(jis.targetDescriptor),
      jis.invokeStatementType.id, jis.instructionPc)

  private def toFieldAccessRepr(jfas: JavaFieldAccessStatement, parentId: Long, typeNameLookup: Map[String, Long]): JavaFieldAccessRepr =
    JavaFieldAccessRepr(parentId, typeNameLookup(jfas.targetFieldTypeName), typeNameLookup(jfas.targetTypeName),
      jfas.fieldAccessType.id, jfas.instructionPc)

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

  override def hasProgram(gav: String): Boolean = {
    val queryFuture = db.run(entitiesTable.filter(e => e.kind === SoftwareEntityKind.Program.id).filter(_.name === gav).exists.result)
    Await.result(queryFuture, 10.seconds)
  }
}
