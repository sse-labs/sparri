package de.tudo.sse.spareuse.execution.storage.impl


import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisRunData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaFieldAccessType, JavaInvocationType, JavaInvokeStatement, JavaLibrary, JavaMethod, JavaPackage, JavaProgram}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.storage.postgresql.JavaDefinitions.{JavaClassRepr, JavaClasses, JavaFieldAccessRepr, JavaFieldAccessStatements, JavaInvocationRepr, JavaInvocationStatements, JavaMethodRepr, JavaMethods}
import de.tudo.sse.spareuse.core.storage.postgresql.{AnalysisRunInputs, SoftwareAnalyses, SoftwareAnalysisRepr, SoftwareAnalysisRuns, SoftwareEntities, SoftwareEntityRepr}
import de.tudo.sse.spareuse.execution.storage.DataAccessor
import org.slf4j.{Logger, LoggerFactory}
import slick.lifted.TableQuery
import slick.jdbc.PostgresProfile.api._

import java.util.HexFormat
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

  override def initializeAnalysisTables(): Unit = {
    val setupAction = DBIO.seq(analysesTable.schema.createIfNotExists, analysisRunsTable.schema.createIfNotExists,
      analysisRunInputsTable.schema.createIfNotExists)
    //TODO: Initialize all Tables once they are defined

    val setupF = db.run(setupAction)

    Await.ready(setupF, longActionTimeout)
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
        getAllChildEntitiesOf(rootEntityRepr._1, resolutionScope) else Success(Seq.empty)

      allChildEntities match {
        case Success(childEntities) =>
          buildEntityObjectTree(Seq(rootEntityRepr) ++ childEntities, rootEntityRepr._1)
        case Failure(ex) =>
          Failure(ex)
      }
    }
  }



  private def getEntityRepr(ident: String,
                            kind: SoftwareEntityKind): Try[SoftwareEntityRepr] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === ident).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(entity) if entity._5 == kind.id =>
       entity
      case _ =>
        throw new IllegalArgumentException(s"Entity of kind $kind with FQ $ident not found")
    }
  }

  private def getAllChildEntitiesOf(entityDbId: Long, resolutionScope: SoftwareEntityKind): Try[Seq[SoftwareEntityRepr]] = Try {
    val directChildren = getChildEntitiesOf(entityDbId).get

    if(directChildren.isEmpty || directChildren.head._5 == resolutionScope.id)
      directChildren
    else
      directChildren ++ directChildren.flatMap( c => getAllChildEntitiesOf(c._1, resolutionScope).get)
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

  private def buildEntityObjectTree(reprs: Seq[SoftwareEntityRepr], rootId: Long): Try[SoftwareEntityData] = Try {

    //Retrieve all data from extension tables
    val allClassesData: Map[Long, JavaClassRepr] = getClassTableData(reprs.filter(repr => repr._5 == SoftwareEntityKind.Class.id).map(_._1)).map(r => (r._1, r)).toMap
    val allMethodsData: Map[Long, JavaMethodRepr] = getMethodTableData(reprs.filter(repr => repr._5 == SoftwareEntityKind.Method.id).map(_._1)).map(r => (r._1, r)).toMap
    val allInvokeStmtData: Map[Long, JavaInvocationRepr] = getInvocationTableData(reprs.filter(repr => repr._5 == SoftwareEntityKind.InvocationStatement.id).map(_._1)).map(r => (r._1, r)).toMap
    val allFieldAccessStmtData: Map[Long, JavaFieldAccessRepr] = getFieldAccessTableData(reprs.filter(repr => repr._5 == SoftwareEntityKind.FieldAccessStatement.id).map(_._1)).map(r => (r._1, r)).toMap

    val lookup = reprs.map { repr =>
      val obj = repr._5 match {
        case 0 => // LIBRARY
          new JavaLibrary(repr._2, repr._6)

        case 1 => // PROGRAM
          val hashedBytes: Array[Byte] = repr._8.map(HexFormat.of().parseHex).getOrElse(Array.empty)
          new JavaProgram(repr._2, repr._2, repr._6, hashedBytes)

        case 2 => // PACKAGE
          new JavaPackage(repr._2, repr._6)

        case 3 => // CLASS
          val hashedBytes: Array[Byte] = repr._8.map(HexFormat.of().parseHex).getOrElse(Array.empty)
          val classData = allClassesData(repr._1)
          new JavaClass(repr._2, classData._2, classData._3, repr._6, hashedBytes)

        case 4 => // METHOD
          val methodData = allMethodsData(repr._1)

          val paramTypeNames = methodData._4.split(",")

          if(paramTypeNames.length != methodData._3)
            throw new IllegalStateException("Corrupt database, parameter count does not match actual parameters")

          new JavaMethod(repr._2, methodData._2, paramTypeNames, repr._6)

        case 5 => // INVOCATION
          val invokeData = allInvokeStmtData(repr._1)
          val invocationType = JavaInvocationType.fromId(invokeData._5)

          new JavaInvokeStatement(repr._2, invokeData._2, invokeData._3, invokeData._4, invocationType, invokeData._6, repr._6)

        case 6 => // FIELD ACCESS
          val fieldAccessData = allFieldAccessStmtData(repr._1)
          val accessType = JavaFieldAccessType.fromId(fieldAccessData._4)

          new JavaFieldAccessStatement(repr._2, fieldAccessData._2, fieldAccessData._3, accessType, fieldAccessData._5, repr._6)
      }

      (repr._1, (obj, repr._7))
    }.toMap

    lookup.keys.foreach{ eid =>
      val entity: SoftwareEntityData = lookup(eid)._1
      val parentIdOpt: Option[Long] = lookup(eid)._2

      if(parentIdOpt.isDefined) entity.setParent(lookup(parentIdOpt.get)._1)
    }

    lookup(rootId)._1
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

    val analysisRuns = if(includeRuns) getAnalysisRuns(repr._1, analysisName, analysisVersion, includeResults = false)
      else Set.empty[AnalysisRunData]

    val inputLanguages = repr._7.split(",").toSet
    val inputEntityKind = SoftwareEntityKind.fromId(repr._9)

    // TODO: Input Formats
    AnalysisData(repr._2, repr._3, repr._4, repr._5, repr._6, inputLanguages, repr._8, null, inputEntityKind, analysisRuns)
  }

  private def getInputsForRun(analysisRunId: Long): Set[SoftwareEntityData] = {
    val queryF = db.run {
        val runIdToInput = for { (ri, i) <- analysisRunInputsTable join entitiesTable on (_.inputEntityID === _.id) } yield (ri.analysisRunID, i )

        runIdToInput.filter(t => t._1 === analysisRunId).map(t => t._2).result
      }
      .map{ entities =>
        // TODO: Convert to *flat* entity representation
        ???
      }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout)
  }

  private def getAnalysisRuns(analysisId: Long, analysisName: String, analysisVersion: String, includeResults: Boolean): Set[AnalysisRunData] = {

    val queryF = db
      .run(analysisRunsTable.filter(r => r.parentID === analysisId).result)
      .map{ allRuns =>
        allRuns.map { run =>
          //TODO: Timestamp, logs, results if requested
          AnalysisRunData(null, Array.empty, run.config, run.isRevoked, getInputsForRun(run.id), Set.empty, analysisName, analysisVersion)
        }
      }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  override def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean): Try[Set[AnalysisRunData]] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion)._1

    getAnalysisRuns(analysisId, analysisName, analysisVersion, includeResults)
  }
}
