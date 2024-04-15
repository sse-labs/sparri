package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaFieldAccessRepr, JavaInvocationRepr, JavaMethodRepr, JavaProgramRepr}
import org.anon.spareuse.core.storage.EntityAccessor
import org.anon.spareuse.core.utils.fromHex
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait PostgresEntityAccessor extends EntityAccessor {
  this: PostgresSparriSupport =>

  implicit val executor: ExecutionContext

  override def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean = {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === ident && swe.kind === kind.id).exists.result)

    Await.result(queryF, simpleQueryTimeout)
  }

  override def hasEntity(ident: String): Boolean = {
    Try {
      val queryF = db.run(entitiesTable.filter(e => e.qualifier === ident).exists.result)
      Await.result(queryF, simpleQueryTimeout)
    } match {
      case Success(value) => value
      case Failure(ex) =>
        log.error("Failed to perform DB lookup", ex)
        false
    }
  }

  override def getEntityKind(entityIdent: String): Try[SoftwareEntityKind] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === entityIdent).take(1).map(_.kind).result)

    SoftwareEntityKind.fromId(Await.result(queryF, simpleQueryTimeout).head)
  }

  override def getEntityChildren(uid: String, skip: Int, limit: Int): Try[Seq[SoftwareEntityData]] = Try {
    val parentEntityId = getEntityId(uid)

    val queryF = db.run(entitiesTable.filter(swe => swe.parentID === parentEntityId).sortBy(_.id).drop(skip).take(limit).result)

    val entityRepResult = Await.result(queryF, longActionTimeout)

    buildEntities(entityRepResult)
  }

  override def getEntity(ident: String, resolutionScope: SoftwareEntityKind): Future[SoftwareEntityData] = {

    def getReprFor(ident: String): Future[SoftwareEntityRepr] = {
      db.run(entitiesTable.filter(_.qualifier === ident).take(1).result).map(_.head)
    }

    def getEntitiesWhereParentIn(parentIds: Seq[Long]): Future[Seq[SoftwareEntityRepr]] = {
      db.run(entitiesTable.filter(_.parentID inSet parentIds).result)
    }

    def getAllParentEntities(currentEntity: SoftwareEntityRepr): Future[Seq[SoftwareEntityRepr]] = {

      if (currentEntity.parentId.isDefined) {
        db
          .run(entitiesTable.filter(_.id === currentEntity.parentId.get).take(1).result)
          .map(_.head)
          .flatMap { parentRepr =>
            getAllParentEntities(parentRepr).map(result => result ++ Seq(parentRepr))
          }
      } else {
        Future.successful(Seq.empty)
      }
    }


    // Build Future that collects all entity representations of this entity's tree (parents and children)
    val allEntityReprsAndRootEntityRepr =
      getReprFor(ident) // Get the root element's representation
        .flatMap { rootRepr =>

          // Calculate how many levels (downwards, children) we have to resolve. Zero means we only resolve the level of the root entity
          val currKindDepth = Math.min(rootRepr.kindId, 5)
          val resolutionDepth = Math.min(resolutionScope.id, 5)
          val levelsToResolve = Math.max(resolutionDepth - currKindDepth, 0)

          // Start with a future that only contains the root element's representation (always resolved)
          var currFuture = Future((Seq(rootRepr), Seq(rootRepr)))

          // For each level to resolve: Chain an additional future that takes the new representations of the last level and looks for their children.
          // Concat so that we end up with a set of all child representations for the given number of levels to resolve.
          Range(0, levelsToResolve).foreach { _ =>
            currFuture = currFuture.flatMap { case (newEntities, allEntities) =>
              getEntitiesWhereParentIn(newEntities.map(_.id)).map(childEntities => (childEntities, childEntities ++ allEntities))
            }
          }

          // Chain a future that resolves all parent representations for the root element and add them to the set of all child representations
          currFuture
            .flatMap { case (_, allEntities) =>
              getAllParentEntities(rootRepr).map(allParents => (allParents ++ allEntities, rootRepr))
            }
        }

    allEntityReprsAndRootEntityRepr
      .flatMap { case (allRepresentations, rootRepresentation) =>
        specializeAll(allRepresentations).map(allEntities => allEntities(rootRepresentation.id))
      }

  }


  case class SpecificationTables(programT: Map[Long, JavaProgramRepr],
                                 classT: Map[Long, JavaClassRepr],
                                 methodT: Map[Long, JavaMethodRepr],
                                 invocationT: Map[Long, JavaInvocationRepr],
                                 fieldAccessT: Map[Long, JavaFieldAccessRepr]) {
    def withClasses(classes: Seq[JavaClassRepr]): SpecificationTables =
      SpecificationTables(programT, classes.map(c => (c._1, c)).toMap, methodT, invocationT, fieldAccessT)

    def withMethods(methods: Seq[JavaMethodRepr]): SpecificationTables =
      SpecificationTables(programT, classT, methods.map(m => (m._1, m)).toMap, invocationT, fieldAccessT)

    def withInvocations(invocations: Seq[JavaInvocationRepr]): SpecificationTables =
      SpecificationTables(programT, classT, methodT, invocations.map(i => (i._1, i)).toMap, fieldAccessT)

    def withFieldAccesses(fieldAccesses: Seq[JavaFieldAccessRepr]): SpecificationTables =
      SpecificationTables(programT, classT, methodT, invocationT, fieldAccesses.map(f => (f._1, f)).toMap)
  }

  object SpecificationTables {
    def fromPrograms(programs: Seq[JavaProgramRepr]): SpecificationTables =
      SpecificationTables(programs.map(p => (p._1, p)).toMap, null, null, null, null)
  }

  private[storage] def getSpecificationTables(allRepresentations: Seq[SoftwareEntityRepr]): Future[SpecificationTables] = {
    val allProgramIds = allRepresentations.filter(_.kindId == SoftwareEntityKind.Program.id).map(_.id).toSet
    val allClassIds = allRepresentations.filter(_.kindId == SoftwareEntityKind.Class.id).map(_.id).toSet
    val allMethodIds = allRepresentations.filter(_.kindId == SoftwareEntityKind.Method.id).map(_.id).toSet
    val allInvocationIds = allRepresentations.filter(_.kindId == SoftwareEntityKind.InvocationStatement.id).map(_.id).toSet
    val allFieldAccessIds = allRepresentations.filter(_.kindId == SoftwareEntityKind.FieldAccessStatement.id).map(_.id).toSet

    db
      .run(javaProgramsTable.filter(_.id inSet allProgramIds).result)
      .map(SpecificationTables.fromPrograms)
      .flatMap { specTables =>
        db
          .run(javaClassesTable.filter(_.id inSet allClassIds).result)
          .map(specTables.withClasses)
      }
      .flatMap { specTables =>
        db
          .run(javaMethodsTable.filter(_.id inSet allMethodIds).result)
          .map(specTables.withMethods)
      }
      .flatMap { specTables =>
        db
          .run(javaInvocationsTable.filter(_.id inSet allInvocationIds).result)
          .map(specTables.withInvocations)
      }
      .flatMap { specTables =>
        db
          .run(javaFieldAccessesTable.filter(_.id inSet allFieldAccessIds).result)
          .map(specTables.withFieldAccesses)
      }


  }

  private[storage] def specializeAll(representations: Seq[SoftwareEntityRepr]): Future[Map[Long, SoftwareEntityData]] = {
    getSpecificationTables(representations)
      .map { specificationTables =>

        val parentRelation = representations.map(r => (r.id, r.parentId)).toMap

        val specializedEntityMap = representations.map { entityRepr =>
          val specializedEntity = SoftwareEntityKind.fromId(entityRepr.kindId) match {
            case SoftwareEntityKind.Library =>
              JavaConverter.toLib(entityRepr)
            case SoftwareEntityKind.Program =>
              JavaConverter.toProgram(entityRepr, specificationTables.programT(entityRepr.id))
            case SoftwareEntityKind.Package =>
              JavaConverter.toPackage(entityRepr)
            case SoftwareEntityKind.Class =>
              JavaConverter.toClass(entityRepr, specificationTables.classT(entityRepr.id))
            case SoftwareEntityKind.Method =>
              JavaConverter.toMethod(entityRepr, specificationTables.methodT(entityRepr.id))
            case SoftwareEntityKind.InvocationStatement =>
              JavaConverter.toInvocation(entityRepr, specificationTables.invocationT(entityRepr.id))
            case SoftwareEntityKind.FieldAccessStatement =>
              JavaConverter.toFieldAccess(entityRepr, specificationTables.fieldAccessT(entityRepr.id))
            case SoftwareEntityKind.NewInstanceStatement =>
              JavaConverter.toNewInstanceCreation(entityRepr)
          }

          (entityRepr.id, specializedEntity)
        }.toMap

        parentRelation.filter(_._2.isDefined).foreach { case (entityId, parentIdOpt) =>
          specializedEntityMap(entityId).setParent(specializedEntityMap(parentIdOpt.get))
        }

        specializedEntityMap
      }

  }

  override def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[String]): Try[Seq[GenericEntityData]] = Try {

    val queryF = if (parentFilter.isDefined) buildEntityQueryWithParentFilter(limit, skip, parentFilter.get, kindFilter)
    else if (kindFilter.isDefined) db.run(entitiesTable.filter(e => e.kind === kindFilter.get.id).sortBy(_.id).drop(skip).take(limit).result)
    else db.run(entitiesTable.sortBy(_.id).drop(skip).take(limit).result)

    Await.result(queryF, longActionTimeout).map(toGenericEntityData)

  }

  private def buildEntityQueryWithParentFilter(limit: Int, skip: Int, parentUid: String, kindFilter: Option[SoftwareEntityKind]) = {

    val joinQuery = if (kindFilter.isEmpty) {
      for {
        (entities, e2) <- entitiesTable join entitiesTable on ((a, b) => a.parentID === b.id)
        if entities.parentID.isDefined && e2.qualifier === parentUid
      } yield entities
    } else {
      val requiredKindId = kindFilter.get.id

      for {
        (entities, e2) <- entitiesTable join entitiesTable on ((a, b) => a.parentID === b.id)
        if entities.parentID.isDefined && e2.qualifier === parentUid && entities.kind === requiredKindId
      } yield entities

    }


    db.run(joinQuery.sortBy(_.id).drop(skip).take(limit).result)
  }

  /*
    TODO: GET RID OF / ADAPT THOSE REDUNDANT / BLOCKING HELPER METHODS BELOW
     */

  protected[storage] def toGenericEntityData(repr: SoftwareEntityRepr): GenericEntityData = {

    idToIdentifierCache.pushValue(repr.id, repr.fqn)

    val parentIdent = repr.parentId.map(parentId => idToIdentifierCache.getWithCache(parentId, () => getUidForEntityId(parentId)))

    new GenericEntityData(repr.name, repr.language, SoftwareEntityKind.fromId(repr.kindId),
      repr.repository, repr.hexHash.map(fromHex), repr.fqn, parentIdent)
  }

  protected[storage] def getUidForEntityId(id: Long): String = {
    val queryF = db.run(entitiesTable.filter(e => e.id === id).map(e => e.qualifier).take(1).result)

    Await.result(queryF, simpleQueryTimeout).head
  }

  protected[storage] def getEntityId(qualifier: String): Long = {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === qualifier).take(1).map(_.id).result)

    Await.result(queryF, simpleQueryTimeout).head
  }

  protected[storage] def getEntityRepr(ident: String,
                            kind: SoftwareEntityKind): Try[SoftwareEntityRepr] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === ident).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(entity) if entity.kindId == kind.id =>
        entity
      case _ =>
        throw new IllegalArgumentException(s"Entity of kind $kind with FQ $ident not found")
    }
  }

  private def getProgramTableData(idsToRetrieve: Seq[Long]): Seq[JavaProgramRepr] = {
    if (idsToRetrieve.isEmpty) return Seq.empty
    val queryF = db.run(javaProgramsTable.filter(jp => jp.id inSet idsToRetrieve).result)

    Await.result(queryF, simpleQueryTimeout)
  }

  private def getClassTableData(idsToRetrieve: Seq[Long]): Seq[JavaClassRepr] = {
    if (idsToRetrieve.isEmpty) return Seq.empty
    val queryF = db.run(javaClassesTable.filter(jc => jc.id inSet idsToRetrieve).result)

    Await.result(queryF, simpleQueryTimeout)
  }

  private def getMethodTableData(idsToRetrieve: Seq[Long]): Seq[JavaMethodRepr] = {
    if (idsToRetrieve.isEmpty) return Seq.empty
    val queryF = db.run(javaMethodsTable.filter(jm => jm.id inSet idsToRetrieve).result)

    Await.result(queryF, simpleQueryTimeout)
  }

  private def getInvocationTableData(idsToRetrieve: Seq[Long]): Seq[JavaInvocationRepr] = {
    if (idsToRetrieve.isEmpty) return Seq.empty
    val queryF = db.run(javaInvocationsTable.filter(ji => ji.id inSet idsToRetrieve).result)

    Await.result(queryF, simpleQueryTimeout)
  }

  private def getFieldAccessTableData(idsToRetrieve: Seq[Long]): Seq[JavaFieldAccessRepr] = {
    if (idsToRetrieve.isEmpty) return Seq.empty
    val queryF = db.run(javaFieldAccessesTable.filter(jfa => jfa.id inSet idsToRetrieve).result)

    Await.result(queryF, simpleQueryTimeout)
  }


  protected[storage] def buildEntities(reprs: Seq[SoftwareEntityRepr]): Seq[SoftwareEntityData] = buildEntitiesIdMap(reprs).values.toSeq

  protected[storage] def buildEntitiesIdMap(reprs: Seq[SoftwareEntityRepr]): Map[Long, SoftwareEntityData] = {
    //Retrieve all data from extension tables
    val allProgramsData: Map[Long, JavaProgramRepr] = getProgramTableData(reprs.filter(_.kindId == SoftwareEntityKind.Program.id).map(_.id)).map(r => (r._1, r)).toMap
    val allClassesData: Map[Long, JavaClassRepr] = getClassTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.Class.id).map(_.id)).map(r => (r._1, r)).toMap
    val allMethodsData: Map[Long, JavaMethodRepr] = getMethodTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.Method.id).map(_.id)).map(r => (r._1, r)).toMap
    val allInvokeStmtData: Map[Long, JavaInvocationRepr] = getInvocationTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.InvocationStatement.id).map(_.id)).map(r => (r._1, r)).toMap
    val allFieldAccessStmtData: Map[Long, JavaFieldAccessRepr] = getFieldAccessTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.FieldAccessStatement.id).map(_.id)).map(r => (r._1, r)).toMap

    reprs.map { repr =>
      val entityObj = SoftwareEntityKind.fromId(repr.kindId) match {
        case SoftwareEntityKind.Library => JavaConverter.toLib(repr)
        case SoftwareEntityKind.Program => JavaConverter.toProgram(repr, allProgramsData(repr.id))
        case SoftwareEntityKind.Package => JavaConverter.toPackage(repr)
        case SoftwareEntityKind.Class => JavaConverter.toClass(repr, allClassesData(repr.id))
        case SoftwareEntityKind.Method => JavaConverter.toMethod(repr, allMethodsData(repr.id))
        case SoftwareEntityKind.InvocationStatement => JavaConverter.toInvocation(repr, allInvokeStmtData(repr.id))
        case SoftwareEntityKind.FieldAccessStatement => JavaConverter.toFieldAccess(repr, allFieldAccessStmtData(repr.id))
        case SoftwareEntityKind.NewInstanceStatement => JavaConverter.toNewInstanceCreation(repr)
      }

      (repr.id, entityObj)
    }.toMap
  }
}
