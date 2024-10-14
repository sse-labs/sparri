package org.anon.spareuse.core.storage.postgresql

import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import org.anon.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassInterface, JavaClassRepr, JavaFieldAccessRepr, JavaInvocationRepr, JavaMethodDescriptor, JavaMethodRepr, JavaProgramRepr, JavaTypeName}
import org.anon.spareuse.core.storage.EntityAccessor
import org.anon.spareuse.core.utils.fromHex
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait PostgresEntityAccessor extends EntityAccessor {
  this: PostgresSparriSupport =>

  implicit val executor: ExecutionContext

  private final val statementKindIds = Set(SoftwareEntityKind.InvocationStatement.id, SoftwareEntityKind.FieldAccessStatement.id, SoftwareEntityKind.NewInstanceStatement.id)

  def getProgramEntityId(gav: String): Option[Long] = {
    Await.result(getProgramIdF(gav), simpleQueryTimeout)
  }

  def getClassEntityId(gav: String, classFqn: String): Option[Long] = {
    Await.result(getClassIdF(gav, classFqn), simpleQueryTimeout)
  }

  def getMethodEntityId(gav: String, classFqn: String, methodIdent: String): Option[Long] = {
    Await.result(getMethodIdF(gav, classFqn, methodIdent), simpleQueryTimeout)
  }

  def getLibraryIdF(ga: String): Future[Option[Long]] = {
    db.run(entitiesTable.filter(swe => swe.parentID.isEmpty && swe.identifier === ga).take(1).map(_.id).result).map(_.headOption)
  }

  def getProgramIdF(gav: String): Future[Option[Long]] = {
    val ident = MavenIdentifier.fromGAV(gav).get
    val ga = ident.toGA
    val v = ident.version

    getLibraryIdF(ga).flatMap{
      case Some(libId) =>
        db.run(entitiesTable.filter(swe => swe.parentID === libId && swe.identifier === v).take(1).map(_.id).result).map(_.headOption)
      case None => Future.successful(None)
    }
  }

  def getClassIdF(gav: String, classFqn: String): Future[Option[Long]] = {
    val packageName = classFqn.substring(classFqn.lastIndexOf("/") + 1)
    getProgramIdF(gav).flatMap{
      case Some(progId) =>
        db.run(entitiesTable.filter(swe => swe.parentID === progId && swe.identifier === packageName).take(1).map(_.id).result).map(_.headOption).flatMap{
          case Some(packageId) =>
            db.run(entitiesTable.filter(swe => swe.parentID === packageId && swe.identifier === classFqn).take(1).map(_.id).result).map(_.headOption)
          case None => Future.successful(None)
        }
      case None => Future.successful(None)
    }
  }

  def getMethodIdF(gav: String, classFqn: String, methodIdent: String): Future[Option[Long]] = {
    getClassIdF(gav, classFqn).flatMap{
      case Some(classId) =>
        db.run(entitiesTable.filter(swe => swe.parentID === classId && swe.identifier === methodIdent).take(1).map(_.id).result).map(_.headOption)
      case None => Future.successful(None)
    }
  }

  /**
   * A lookup method that is not dependent on the fully-explicit FQN field for entities - SLOW ...
   * @param ident Fully-Explicit entity FQN (old format, "!"-separated)
   * @return Option holding the EntityRepr if it exists
   */
  private def getEntityDataFromIdent(ident: String): Option[SoftwareEntityRepr] = {
    val identParts = ident.split("!").zipWithIndex

    var query: Query[SoftwareEntities, SoftwareEntities#TableElementType, Seq] = null

    for((currIdent, currLevel) <- identParts){

      if(query == null){
        query = if (currLevel < SoftwareEntityKind.InvocationStatement.id) {
          entitiesTable.filter(swe => swe.kind === currLevel && swe.name === currIdent).take(1)
        } else {
          entitiesTable.filter(swe => swe.name === currIdent && swe.kind.inSet(statementKindIds)).take(1)
        }
      } else {
        val kindSet = if(currLevel < SoftwareEntityKind.InvocationStatement.id) Set(currLevel) else statementKindIds
        query = (for{ (_, child) <- query join entitiesTable on (_.id === _.parentID) if child.name === currIdent && child.kind.inSet(kindSet) } yield child).take(1)
      }
    }

    val result = Await.result(db.run(query.result), longActionTimeout)

    result.headOption
  }


  override def hasEntity(eid: Long, kind: SoftwareEntityKind): Boolean = {
    val queryF = db.run(entitiesTable.filter(swe => swe.id === eid && swe.kind === kind.id).exists.result)

    Await.result(queryF, simpleQueryTimeout)
  }

  override def hasEntity(eid: Long): Boolean = {
    Try {
      val queryF = db.run(entitiesTable.filter(e => e.id === eid).exists.result)
      Await.result(queryF, simpleQueryTimeout)
    } match {
      case Success(value) => value
      case Failure(ex) =>
        log.error("Failed to perform DB lookup", ex)
        false
    }
  }

  override def getEntityKind(eid: Long): Try[SoftwareEntityKind] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.id === eid).take(1).map(_.kind).result)

    SoftwareEntityKind.fromId(Await.result(queryF, simpleQueryTimeout).head)
  }

  override def getEntityChildren(eid: Long, skip: Int, limit: Int): Try[Seq[SoftwareEntityData]] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.parentID === eid).sortBy(_.id).drop(skip).take(limit).result)

    val entityRepResult = Await.result(queryF, longActionTimeout)

    buildEntities(entityRepResult)
  }

  override def getEntity(eid: Long, resolutionScope: SoftwareEntityKind): Future[SoftwareEntityData] = {

    def getReprFor(id: Long): Future[SoftwareEntityRepr] = {
      db.run(entitiesTable.filter(_.id === id).take(1).result).map(_.head)
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
      getReprFor(eid) // Get the root element's representation
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
                                 classInterfaceT: Map[Long, Seq[Long]],
                                 methodT: Map[Long, JavaMethodRepr],
                                 invocationT: Map[Long, JavaInvocationRepr],
                                 fieldAccessT: Map[Long, JavaFieldAccessRepr],
                                 typeNameT: Map[Long, String],
                                 descriptorT: Map[Long, String]) {
    def withClasses(classes: Seq[JavaClassRepr]): SpecificationTables =
      SpecificationTables(programT, classes.map(c => (c.id, c)).toMap, classInterfaceT, methodT, invocationT, fieldAccessT, typeNameT, descriptorT)

    def withClassInterfaces(classInterfaces: Seq[JavaClassInterface]): SpecificationTables = {
      SpecificationTables(programT, classT, classInterfaces.map(ci => (ci.classId, ci.interfaceId)).groupMap(_._1)(_._2), methodT, invocationT, fieldAccessT, typeNameT, descriptorT)
    }

    def withMethods(methods: Seq[JavaMethodRepr]): SpecificationTables =
      SpecificationTables(programT, classT, classInterfaceT, methods.map(m => (m.id, m)).toMap, invocationT, fieldAccessT, typeNameT, descriptorT)

    def withInvocations(invocations: Seq[JavaInvocationRepr]): SpecificationTables =
      SpecificationTables(programT, classT, classInterfaceT, methodT, invocations.map(i => (i.id, i)).toMap, fieldAccessT, typeNameT, descriptorT)

    def withFieldAccesses(fieldAccesses: Seq[JavaFieldAccessRepr]): SpecificationTables =
      SpecificationTables(programT, classT, classInterfaceT, methodT, invocationT, fieldAccesses.map(f => (f.id, f)).toMap, typeNameT, descriptorT)

    def withTypeNames(names: Seq[JavaTypeName]): SpecificationTables = {
      SpecificationTables(programT, classT, classInterfaceT, methodT, invocationT, fieldAccessT, names.map(n => (n.id, n.name)).toMap, descriptorT)
    }

    def withDescriptors(desciptors: Seq[JavaMethodDescriptor]): SpecificationTables = {
      SpecificationTables(programT, classT, classInterfaceT, methodT, invocationT, fieldAccessT, typeNameT, desciptors.map(d => (d.id, d.descriptor)).toMap)
    }
  }

  object SpecificationTables {
    def fromPrograms(programs: Seq[JavaProgramRepr]): SpecificationTables =
      SpecificationTables(programs.map(p => (p._1, p)).toMap, null, null, null, null, null, null, null)
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
      .flatMap{ specTables =>
        db
          .run(javaClassInterfacesTable.filter(_.classId inSet allClassIds).result)
          .map(specTables.withClassInterfaces)
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
      .flatMap{ specTables =>
        val allNameIds =
          specTables.classT.values.flatMap(cr => Set(cr.typeNameId) ++ cr.superTypeNameId.toSet) ++
          specTables.classInterfaceT.values.flatten ++
          specTables.invocationT.values.map(invoke => invoke.declTypeNameId) ++
          specTables.fieldAccessT.values.flatMap(access => Set(access.declTypeNameId, access.fieldTypeNameId))

        db
          .run(typeNameTable.filter(_.id inSet allNameIds).result)
          .map(specTables.withTypeNames)
      }
      .flatMap { specTables =>
        val allDescriptorIds = specTables.methodT.values.map(_.descriptorId) ++ specTables.invocationT.values.map(_.descriptorId)

        db
          .run(descriptorTable.filter(_.id inSet allDescriptorIds).result)
          .map(specTables.withDescriptors)
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
              val classRep = specificationTables.classT(entityRepr.id)
              val interfaces = specificationTables.classInterfaceT.get(classRep.id).map(iSeq => iSeq.map(specificationTables.typeNameT)).getOrElse(Seq.empty).toSet
              JavaConverter.toClass(entityRepr, specificationTables.classT(entityRepr.id), interfaces, specificationTables.typeNameT)
            case SoftwareEntityKind.Method =>
              JavaConverter.toMethod(entityRepr, specificationTables.methodT(entityRepr.id), specificationTables.descriptorT)
            case SoftwareEntityKind.InvocationStatement =>
              JavaConverter.toInvocation(entityRepr, specificationTables.invocationT(entityRepr.id), specificationTables.typeNameT, specificationTables.descriptorT)
            case SoftwareEntityKind.FieldAccessStatement =>
              JavaConverter.toFieldAccess(entityRepr, specificationTables.fieldAccessT(entityRepr.id), specificationTables.typeNameT)
            case SoftwareEntityKind.NewInstanceStatement =>
              JavaConverter.toNewInstanceCreation(entityRepr)
          }

          (entityRepr.id, specializedEntity)
        }.toMap

        parentRelation.filter(_._2.isDefined).foreach { case (entityId, parentIdOpt) =>
          if(specializedEntityMap.contains(parentIdOpt.get))
            specializedEntityMap(entityId).setParent(specializedEntityMap(parentIdOpt.get))
        }

        specializedEntityMap
      }

  }

  override def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[Long]): Future[Seq[SoftwareEntityData]] = {

    val rawEntityQuery = if (parentFilter.isDefined) buildEntityQueryWithParentFilter(limit, skip, parentFilter.get, kindFilter)
    else if (kindFilter.isDefined) db.run(entitiesTable.filter(e => e.kind === kindFilter.get.id).sortBy(_.id).drop(skip).take(limit).result)
    else db.run(entitiesTable.sortBy(_.id).drop(skip).take(limit).result)

    rawEntityQuery
      .flatMap { entityReprs =>
        specializeAll(entityReprs).map(_.values.toSeq)
      }

  }

  private def buildEntityQueryWithParentFilter(limit: Int, skip: Int, parentEid: Long, kindFilter: Option[SoftwareEntityKind]) = {

    val joinQuery = if (kindFilter.isEmpty) {
      for {
        (entities, e2) <- entitiesTable join entitiesTable on ((a, b) => a.parentID === b.id)
        if entities.parentID.isDefined && e2.id === parentEid
      } yield entities
    } else {
      val requiredKindId = kindFilter.get.id

      for {
        (entities, e2) <- entitiesTable join entitiesTable on ((a, b) => a.parentID === b.id)
        if entities.parentID.isDefined && e2.id === parentEid && entities.kind === requiredKindId
      } yield entities

    }


    db.run(joinQuery.sortBy(_.id).drop(skip).take(limit).result)
  }

  /*
    TODO: GET RID OF / ADAPT THOSE REDUNDANT / BLOCKING HELPER METHODS BELOW
     */

  protected[storage] def toGenericEntityData(repr: SoftwareEntityRepr): GenericEntityData = {

    new GenericEntityData(repr.name, repr.language, SoftwareEntityKind.fromId(repr.kindId),
      repr.repository, repr.hexHash.map(fromHex), repr.id, repr.identifier, repr.parentId)
  }

  protected[storage] def getEntityRepr(eid: Long,
                            kind: SoftwareEntityKind): Try[SoftwareEntityRepr] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.id === eid).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(entity) if entity.kindId == kind.id =>
        entity
      case _ =>
        throw new IllegalArgumentException(s"Entity of kind $kind with ID $eid not found")
    }
  }


  protected[storage] def buildEntities(reprs: Seq[SoftwareEntityRepr]): Seq[SoftwareEntityData] = Await.result(specializeAll(reprs), longActionTimeout).values.toSeq
}
