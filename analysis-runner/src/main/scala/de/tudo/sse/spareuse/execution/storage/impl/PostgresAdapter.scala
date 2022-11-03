package de.tudo.sse.spareuse.execution.storage.impl


import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisRunData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaClasses, JavaFieldAccessRepr, JavaFieldAccessStatements, JavaInvocationRepr, JavaInvocationStatements, JavaMethodRepr, JavaMethods}
import de.tudo.sse.spareuse.core.storage.postgresql.{AnalysisRunInputs, JavaConverter, SoftwareAnalyses, SoftwareAnalysisRepr, SoftwareAnalysisRuns, SoftwareEntities, SoftwareEntityRepr, toAnalysisRepr}
import de.tudo.sse.spareuse.execution.analyses.AnalysisRegistry
import de.tudo.sse.spareuse.execution.storage.DataAccessor
import org.slf4j.{Logger, LoggerFactory}
import slick.lifted.TableQuery
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class PostgresAdapter extends DataAccessor {

  protected val log: Logger = LoggerFactory.getLogger(getClass())

  private val simpleQueryTimeout = 5.seconds
  private val longActionTimeout = 20.seconds

  private lazy val db = Database.forConfig("spa-reuse.postgres")

  private val entitiesTable = TableQuery[SoftwareEntities]
  private val javaClassesTable = TableQuery[JavaClasses]
  private val javaMethodsTable = TableQuery[JavaMethods]
  private val javaInvocationsTable = TableQuery[JavaInvocationStatements]
  private val javaFieldAccessesTable = TableQuery[JavaFieldAccessStatements]

  private val analysesTable = TableQuery[SoftwareAnalyses]
  private val analysisRunsTable = TableQuery[SoftwareAnalysisRuns]
  private val analysisRunInputsTable = TableQuery[AnalysisRunInputs]

  override def initializeEntityTables(): Unit = {
    val setupAction = DBIO.seq(entitiesTable.schema.createIfNotExists, javaClassesTable.schema.createIfNotExists,
      javaMethodsTable.schema.createIfNotExists, javaInvocationsTable.schema.createIfNotExists, javaFieldAccessesTable.schema.createIfNotExists)

    val setupF = db.run(setupAction)

    Await.ready(setupF, longActionTimeout)
  }

  override def shutdown(): Unit = {
    db.close()
  }

  override def initializeAnalysisTables(): Unit = {
    //TODO: Initialize all Tables once they are defined. Missing: Results, Formats
    val setupAction = DBIO.seq(analysesTable.schema.createIfNotExists, analysisRunsTable.schema.createIfNotExists,
      analysisRunInputsTable.schema.createIfNotExists)

    val setupF = db.run(setupAction)

    Await.ready(setupF, longActionTimeout)

    AnalysisRegistry.allAnalysisImplementations().foreach(impl => assertAnalysisPresent(impl.analysisData))
  }

  override def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean = {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === ident && swe.kind === kind.id).exists.result)

    Await.result(queryF, simpleQueryTimeout)
  }

  override def getEntity(ident: String,
                         kind: SoftwareEntityKind,
                         resolutionScope: SoftwareEntityKind): Try[SoftwareEntityData] = {

    getEntityRepr(ident, kind).flatMap { rootEntityRepr =>

      // Resolve child entities only if needed
      val allChildEntities = if(SoftwareEntityKind.isLessSpecific(kind, resolutionScope))
        getAllChildEntitiesOf(rootEntityRepr.id, resolutionScope) else Success(Seq.empty)

      allChildEntities match {
        case Success(childEntities) =>
          buildEntityObjectTree(Seq(rootEntityRepr) ++ childEntities, rootEntityRepr.id)
        case Failure(ex) =>
          Failure(ex)
      }
    }
  }


  private def assertAnalysisPresent(data: AnalysisData): Unit = {
    if(!hasAnalysis(data.name, data.version)){
      val insertF = db.run(analysesTable += toAnalysisRepr(data))

      Await.result(insertF, simpleQueryTimeout)
    }
  }

  private def getEntityRepr(ident: String,
                            kind: SoftwareEntityKind): Try[SoftwareEntityRepr] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === ident).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(entity) if entity.kindId == kind.id =>
       entity
      case _ =>
        throw new IllegalArgumentException(s"Entity of kind $kind with FQ $ident not found")
    }
  }

  private def getAllChildEntitiesOf(entityDbId: Long, resolutionScope: SoftwareEntityKind): Try[Seq[SoftwareEntityRepr]] = Try {
    val directChildren = getChildEntitiesOf(entityDbId).get

    if(directChildren.isEmpty || directChildren.head.kindId == resolutionScope.id)
      directChildren
    else
      directChildren ++ directChildren.flatMap( c => getAllChildEntitiesOf(c.id, resolutionScope).get)
  }

  private def getChildEntitiesOf(entityDbId: Long): Try[Seq[SoftwareEntityRepr]] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.parentID === entityDbId).result)

    Await.result(queryF, simpleQueryTimeout)
  }

  private def getClassTableData(idsToRetrieve: Seq[Long]): Seq[JavaClassRepr] = {
    if(idsToRetrieve.isEmpty) return Seq.empty
    val queryF = db.run(javaClassesTable.filter(jc => jc.id inSet idsToRetrieve).result)

    Await.result(queryF, simpleQueryTimeout)
  }

  private def getMethodTableData(idsToRetrieve: Seq[Long]): Seq[JavaMethodRepr] = {
    if(idsToRetrieve.isEmpty) return Seq.empty
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


  private def buildEntities(reprs: Seq[SoftwareEntityRepr]): Seq[SoftwareEntityData] = buildEntitiesIdMap(reprs).values.toSeq
  private def buildEntitiesIdMap(reprs: Seq[SoftwareEntityRepr]): Map[Long, SoftwareEntityData] = {
    //Retrieve all data from extension tables
    val allClassesData: Map[Long, JavaClassRepr] = getClassTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.Class.id).map(_.id)).map(r => (r._1, r)).toMap
    val allMethodsData: Map[Long, JavaMethodRepr] = getMethodTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.Method.id).map(_.id)).map(r => (r._1, r)).toMap
    val allInvokeStmtData: Map[Long, JavaInvocationRepr] = getInvocationTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.InvocationStatement.id).map(_.id)).map(r => (r._1, r)).toMap
    val allFieldAccessStmtData: Map[Long, JavaFieldAccessRepr] = getFieldAccessTableData(reprs.filter(repr => repr.kindId == SoftwareEntityKind.FieldAccessStatement.id).map(_.id)).map(r => (r._1, r)).toMap

    reprs.map { repr =>
      val entityObj = SoftwareEntityKind.fromId(repr.kindId) match {
        case SoftwareEntityKind.Library => JavaConverter.toLib(repr)
        case SoftwareEntityKind.Program => JavaConverter.toProgram(repr)
        case SoftwareEntityKind.Package => JavaConverter.toPackage(repr)
        case SoftwareEntityKind.Class => JavaConverter.toClass(repr, allClassesData(repr.id))
        case SoftwareEntityKind.Method => JavaConverter.toMethod(repr, allMethodsData(repr.id))
        case SoftwareEntityKind.InvocationStatement => JavaConverter.toInvocation(repr, allInvokeStmtData(repr.id))
        case SoftwareEntityKind.FieldAccessStatement => JavaConverter.toFieldAccess(repr, allFieldAccessStmtData(repr.id))
      }

      (repr.id, entityObj)
    }.toMap
  }

  private def buildEntityObjectTree(reprs: Seq[SoftwareEntityRepr], rootId: Long): Try[SoftwareEntityData] = Try {

    val idToObjMap = buildEntitiesIdMap(reprs)

    val parentLookup: Map[Long, Option[Long]] = reprs.map(repr => (repr.id, repr.parentId)).toMap

    reprs.map(_.id).foreach { entityId =>
      val entity = idToObjMap(entityId)
      val parentOpt = parentLookup(entityId).flatMap(parentId => idToObjMap.get(parentId))

      if(parentOpt.isDefined) entity.setParent(parentOpt.get)
    }

    idToObjMap(rootId)
  }

  private def getAnalysisRepr(analysisName: String, analysisVersion: String): SoftwareAnalysisRepr = {
    val queryF = db.run(analysesTable.filter(a => a.name === analysisName && a.version === analysisVersion).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(result) => result
      case None => throw new IllegalStateException(s"Analysis $analysisName:$analysisVersion no present in db")
    }
  }

  override def getAnalysisData(analysisName: String, analysisVersion: String, includeRuns: Boolean = false): Try[AnalysisData] = Try {

    val repr = getAnalysisRepr(analysisName, analysisVersion)

    val analysisRuns = if(includeRuns) getAnalysisRuns(repr.id, analysisName, analysisVersion, includeResults = false)
      else Set.empty[AnalysisRunData]

    // TODO: Input Formats
    repr.toAnalysisData(analysisRuns)
  }

  private def getInputsForRun(analysisRunId: Long): Set[SoftwareEntityData] = {
    val queryF = db.run {
        val runIdToInput = for { (ri, i) <- analysisRunInputsTable join entitiesTable on (_.inputEntityID === _.id) } yield (ri.analysisRunID, i )

        runIdToInput.filter(t => t._1 === analysisRunId).map(t => t._2).result
      }
      .map{ entityReprs => buildEntities(entityReprs) }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  private def getAnalysisRuns(analysisId: Long, analysisName: String, analysisVersion: String, includeResults: Boolean): Set[AnalysisRunData] = {

    val queryF = db
      .run(analysisRunsTable.filter(r => r.parentID === analysisId).result)
      .map{ allRuns =>
        allRuns.map { run =>
          run.toAnalysisRunData(analysisName, analysisVersion, getInputsForRun(run.id))
        }
      }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  override def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean): Try[Set[AnalysisRunData]] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    getAnalysisRuns(analysisId, analysisName, analysisVersion, includeResults)
  }
}
