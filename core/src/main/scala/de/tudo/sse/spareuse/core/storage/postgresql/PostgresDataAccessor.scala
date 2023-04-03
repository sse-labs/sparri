package de.tudo.sse.spareuse.core.storage.postgresql

import de.tudo.sse.spareuse.core.formats.{AnalysisResultFormat, AnyValueFormat, BaseValueFormat, EmptyFormat, EntityReferenceFormat, GraphResultFormat, ListResultFormat, MapResultFormat, NamedPropertyFormat, NumberFormat, ObjectResultFormat, StringFormat}
import de.tudo.sse.spareuse.core.model.RunState.RunState
import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, RunState, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import de.tudo.sse.spareuse.core.storage.DataAccessor
import de.tudo.sse.spareuse.core.storage.postgresql.JavaDefinitions._
import de.tudo.sse.spareuse.core.storage.postgresql.ResultFormatPredef.allPredefFormats
import de.tudo.sse.spareuse.core.utils.{ObjectCache, fromHex}
import org.slf4j.{Logger, LoggerFactory}
import slick.lifted.TableQuery
import slick.jdbc.PostgresProfile.api._
import spray.json.JsonWriter

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class PostgresDataAccessor extends DataAccessor {

  protected val log: Logger = LoggerFactory.getLogger(getClass())

  private val idToIdentifierCache = new ObjectCache[Long,String](maxEntries = 10000)

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
  private val analysisResultsTable = TableQuery[AnalysisResults]
  private val resultFormatsTable = TableQuery[ResultFormats]
  private val nestedResultFormatsTable = TableQuery[NestedResultFormatReferences]
  private val resultValiditiesTable = TableQuery[AnalysisResultValidities]

  private lazy val idReturningAnalysisRunTable = analysisRunsTable returning analysisRunsTable.map(_.id)
  private lazy val idReturningFormatTable = resultFormatsTable returning resultFormatsTable.map(_.id)
  private lazy val idReturningResultTable = analysisResultsTable returning analysisResultsTable.map(_.id)

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
    val setupAction = DBIO.seq(resultFormatsTable.schema.createIfNotExists, nestedResultFormatsTable.schema.createIfNotExists,
      analysesTable.schema.createIfNotExists, analysisRunsTable.schema.createIfNotExists, analysisRunInputsTable.schema.createIfNotExists,
      analysisResultsTable.schema.createIfNotExists, resultValiditiesTable.schema.createIfNotExists)

    val setupF = db.run(setupAction)

    Await.ready(setupF, longActionTimeout)

   storePredefinedFormats()
  }

  override def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean = {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === ident && swe.kind === kind.id).exists.result)

    Await.result(queryF, simpleQueryTimeout)
  }

  override def getEntity(ident: String,
                         kind: SoftwareEntityKind,
                         resolutionScope: SoftwareEntityKind): Try[SoftwareEntityData] = {

    getEntityRepr(ident, kind).flatMap { rootEntityRepr =>

      val parentOptTry = getFullyResolvedParent(rootEntityRepr)

      if(parentOptTry.isFailure) return Failure(parentOptTry.failed.get)

      // Resolve child entities only if needed
      val allChildEntities = if (SoftwareEntityKind.isLessSpecific(kind, resolutionScope))
        getAllChildEntitiesOf(rootEntityRepr.id, resolutionScope) else Success(Seq.empty)

      allChildEntities match {
        case Success(childEntities) =>
          buildEntityObjectTree(Seq(rootEntityRepr) ++ childEntities, rootEntityRepr.id).map { finalEntity =>
            if(parentOptTry.get.isDefined) finalEntity.setParent(parentOptTry.get.get)

            finalEntity
          }

        case Failure(ex) =>
          Failure(ex)
      }
    }
  }

  private def getFullyResolvedParent(repr: SoftwareEntityRepr): Try[Option[SoftwareEntityData]] = Try {

    def getReprById(id: Long): SoftwareEntityRepr = Await.result(db.run(entitiesTable.filter( e => e.id === id).take(1).result), simpleQueryTimeout).head


    repr.parentId.map { parentId =>
      val parentRepr = getReprById(parentId)

      val parentParentOpt = getFullyResolvedParent(parentRepr).get

      val parentEntity = buildEntities(Seq(parentRepr)).head

      if(parentParentOpt.isDefined) parentEntity.setParent(parentParentOpt.get)

      parentEntity
    }
  }

  override def getEntityKind(entityIdent: String): Try[SoftwareEntityKind] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === entityIdent).take(1).map(_.kind).result)

    SoftwareEntityKind.fromId(Await.result(queryF, simpleQueryTimeout).head)
  }

  override def registerIfNotPresent(analysis: AnalysisData): Unit = {
    if (!hasAnalysis(analysis.name, analysis.version)) {

      val formatId = storeResultFormat(analysis.resultFormat)

      val insertF = db.run(analysesTable += toAnalysisRepr(analysis, formatId))

      Await.result(insertF, simpleQueryTimeout)
    }
  }

  private def storePredefinedFormats(): Unit = {
    val allPredefIds = allPredefFormats.map(_.id).toSet

    val resultingFormats = Await.result(db.run(resultFormatsTable.filter( f => f.id inSet allPredefIds).result), simpleQueryTimeout)

    resultingFormats.size match {
      case 0 =>
        val query = DBIO.seq( resultFormatsTable += ResultFormatPredef.StringFormat, resultFormatsTable += ResultFormatPredef.NumberFormat,
          resultFormatsTable += ResultFormatPredef.EntityRefFormat, resultFormatsTable += ResultFormatPredef.EmptyFormat)

        Await.ready(db.run(query), simpleQueryTimeout)
      case x if x == allPredefIds.size =>
        allPredefFormats.foreach { predefF =>
          if(resultingFormats.count( _.equals(predefF)) != 1) throw new IllegalStateException("Illegal database state: Predefined formats are invalid")
        }
      case _ => throw new IllegalStateException("Illegal database state: Predefined Formats are not stored correctly")
    }
  }

  private def getResultFormat(rootId: Long): AnyValueFormat = {

    def getNestedFormats: Seq[NestedResultFormatReference] = {
      val queryF = db.run(nestedResultFormatsTable.filter(n => n.originId === rootId).result)
      Await.result(queryF, simpleQueryTimeout)
    }

    rootId match {
      // Don't need to read from DB for base values
      case ResultFormatPredef.StringFormat.id => StringFormat
      case ResultFormatPredef.NumberFormat.id => NumberFormat
      case ResultFormatPredef.EntityRefFormat.id => EntityReferenceFormat
      case ResultFormatPredef.EmptyFormat.id => EmptyFormat

      // All other ids will be some form of composite format
      case _ =>
        // Get raw format entry from DB, get list of all nested formats
        val rawFormatRepr = Await.result(db.run(resultFormatsTable.filter(f => f.id === rootId).take(1).result), simpleQueryTimeout).head
        val nestedFormats = getNestedFormats

        // Parse nested formats based on format type
        rawFormatRepr.resultType match {
          case ResultType.listTypeId if nestedFormats.size == 1 =>

            val elemFormatNesting = nestedFormats.head
            val elemFormat = getResultFormat(elemFormatNesting.targetId)
            ListResultFormat(elemFormat, elemFormatNesting.description)

          case ResultType.mapTypeId if nestedFormats.size == 2 =>

            val keyFormatNesting = nestedFormats.find(n => n.nestingKind == ResultNestingKind.MapKey.id).get
            val valueFormatNesting = nestedFormats.find(n => n.nestingKind == ResultNestingKind.MapValue.id).get

            val keyFormat = getResultFormat(keyFormatNesting.targetId)
            val valueFormat = getResultFormat(valueFormatNesting.targetId)

            if(!keyFormat.isBaseValue)
              throw new IllegalStateException("Corrupt format definition: Map key needs to be base value")

            MapResultFormat(keyFormat.asInstanceOf[BaseValueFormat], valueFormat, keyFormatNesting.description, valueFormatNesting.description)

          case ResultType.objectTypeId =>

            val objectProperties = nestedFormats.map { nesting =>
              val propFormat = getResultFormat(nesting.targetId)

              if(!propFormat.isInstanceOf[NamedPropertyFormat])
                throw new IllegalStateException("Corrupt format definition: Nested formats in object need to be Named Properties")

              propFormat.asInstanceOf[NamedPropertyFormat]
            }.toSet

            ObjectResultFormat(objectProperties)

          case ResultType.graphTypeId =>

            val nodePropNestings = nestedFormats.filter(n => n.nestingKind == ResultNestingKind.NodeProperty.id)
            val edgePropNestings = nestedFormats.filter(n => n.nestingKind == ResultNestingKind.EdgeProperty.id)

            var nodeDescription = ""

            val nodeProps = nodePropNestings.map{ nesting =>
              if(nodeDescription.equals("")) nodeDescription = nesting.description
              val format = getResultFormat(nesting.targetId)


              if(!format.isInstanceOf[NamedPropertyFormat])
                throw new IllegalStateException("Corrupt format definition: Nested formats in graph nodes need to be Named Properties")

              format.asInstanceOf[NamedPropertyFormat]
            }.toSet

            var edgeDescription = ""

            val edgeProps = edgePropNestings.map{ nesting =>
              if(edgeDescription.equals("")) edgeDescription = nesting.description
              val format = getResultFormat(nesting.targetId)

              if (!format.isInstanceOf[NamedPropertyFormat])
                throw new IllegalStateException("Corrupt format definition: Nested formats in graph edges need to be Named Properties")

              format.asInstanceOf[NamedPropertyFormat]
            }.toSet

            GraphResultFormat(edgeProps, nodeProps, nodeDescription, edgeDescription)

          case ResultType.namedPropTypeId if nestedFormats.size == 1 =>

            val innerFormatNesting = nestedFormats.head

            val innerFormat = getResultFormat(innerFormatNesting.targetId)

            NamedPropertyFormat(rawFormatRepr.identifier, innerFormat, innerFormatNesting.description)
        }


    }

  }

  private def storeResultFormat(format: AnyValueFormat): Long = {
    if(format.isBaseValue){
      format match {
        case StringFormat => ResultFormatPredef.StringFormat.id
        case NumberFormat => ResultFormatPredef.NumberFormat.id
        case EntityReferenceFormat => ResultFormatPredef.EntityRefFormat.id
        case EmptyFormat => ResultFormatPredef.EmptyFormat.id
      }
    } else {

      format match {
        case ListResultFormat(elemF, explanation) =>
          val elementFormatId = storeResultFormat(elemF)

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_LIST", ResultType.ListResult.id)), simpleQueryTimeout)

          Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, elementFormatId, ResultNestingKind.ListElement.id, explanation)), simpleQueryTimeout)

          formatId

        case MapResultFormat(keyF, valueF, keyExplanation, valueExplanation) =>
          val keyFormatId = storeResultFormat(keyF)
          val valueFormatId = storeResultFormat(valueF)

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_MAP", ResultType.MapResult.id)), simpleQueryTimeout)

          val keyAndValueInsert = DBIO.seq(nestedResultFormatsTable += NestedResultFormatReference(formatId, keyFormatId, ResultNestingKind.MapKey.id, keyExplanation),
            nestedResultFormatsTable += NestedResultFormatReference(formatId, valueFormatId, ResultNestingKind.MapValue.id, valueExplanation))

          Await.ready(db.run(keyAndValueInsert), simpleQueryTimeout)

          formatId

        case ObjectResultFormat(propFormats) =>

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_OBJECT", ResultType.ObjectResult.id)), simpleQueryTimeout)

          propFormats.foreach{ propFormat =>

            val propFormatId = storeResultFormat(propFormat)

            Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, propFormatId, ResultNestingKind.ObjectProperty.id, "")), simpleQueryTimeout)
          }

          formatId

        case GraphResultFormat(nodeFormats, edgeFormats, nodeDescription, edgeDescription) =>

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, "CUSTOM_GRAPH", ResultType.GraphResult.id)), simpleQueryTimeout)

          nodeFormats.foreach { nodeProp =>
            val propFormatId = storeResultFormat(nodeProp)

            Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, propFormatId, ResultNestingKind.NodeProperty.id, nodeDescription)), simpleQueryTimeout)
          }

          edgeFormats.foreach { edgeProp =>
            val propFormatId = storeResultFormat(edgeProp)

            Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, propFormatId, ResultNestingKind.EdgeProperty.id, edgeDescription)), simpleQueryTimeout)
          }

          formatId

        case NamedPropertyFormat(propertyName, propertyFormat, explanation) =>
          val innerFormatId = storeResultFormat(propertyFormat)

          val formatId = Await.result(db.run(idReturningFormatTable += ResultFormat(-1, propertyName, ResultType.NamedPropertyResult.id)), simpleQueryTimeout)

          Await.ready(db.run(nestedResultFormatsTable += NestedResultFormatReference(formatId, innerFormatId, ResultNestingKind.NamedProperty.id, explanation)), simpleQueryTimeout)

          formatId
      }
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

    if (directChildren.isEmpty || directChildren.head.kindId == resolutionScope.id)
      directChildren
    else
      directChildren ++ directChildren.flatMap(c => getAllChildEntitiesOf(c.id, resolutionScope).get)
  }

  private def getChildEntitiesOf(entityDbId: Long): Try[Seq[SoftwareEntityRepr]] = Try {
    val queryF = db.run(entitiesTable.filter(swe => swe.parentID === entityDbId).result)

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

      if (parentOpt.isDefined) entity.setParent(parentOpt.get)
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

    val analysisRuns = if (includeRuns) getAnalysisRuns(repr.id, analysisName, analysisVersion, includeResults = false)
    else Set.empty[AnalysisRunData]

    getResultFormat(repr.formatId) match {
      case f: AnalysisResultFormat =>
        repr.toAnalysisData(f, analysisRuns)
      case _ =>
        throw new IllegalStateException("Corrupt format definition: Analysis result format must not be base value")
    }
  }


  override def getAnalyses(includeRuns: Boolean = false, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisData]] = Try {

    val queryF = db.run(analysesTable.sortBy(_.id).drop(skip).take(limit).result)

    Await
      .result(queryF, simpleQueryTimeout)
      .map{ analysisRepr =>
        val analysisRuns = if (includeRuns) getAnalysisRuns(analysisRepr.id, analysisRepr.name, analysisRepr.version, includeResults = false)
          else Set.empty[AnalysisRunData]

        getResultFormat(analysisRepr.formatId) match {
          case f: AnalysisResultFormat =>
            analysisRepr.toAnalysisData(f, analysisRuns)
          case _ =>
            throw new IllegalStateException("Corrupt format definition: Analysis result format must not be base value")
        }
      }.toSet
  }

  private def getInputsForRun(analysisRunId: Long): Set[SoftwareEntityData] = {
    val queryF = db.run {
      val runIdToInput = for {(ri, i) <- analysisRunInputsTable join entitiesTable on (_.inputEntityID === _.id)} yield (ri.analysisRunID, i)

      runIdToInput.filter(t => t._1 === analysisRunId).map(t => t._2).result
    }
      .map { entityReprs => buildEntities(entityReprs) }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  private def getAnalysisRuns(analysisId: Long, analysisName: String, analysisVersion: String, includeResults: Boolean): Set[AnalysisRunData] = {

    val queryF = db
      .run(analysisRunsTable.filter(r => r.parentID === analysisId).result)
      .map { allRuns =>
        allRuns.map { run =>
          val results = if (includeResults) {
            getRunResultsAsJSON(run.uid).get
          } else Set.empty[AnalysisResultData]

          run.toAnalysisRunData(analysisName, analysisVersion, getInputsForRun(run.id), results)
        }
      }(db.ioExecutionContext)

    Await.result(queryF, longActionTimeout).toSet
  }

  override def getAnalysisRuns(analysisName: String, analysisVersion: String, includeResults: Boolean): Try[Set[AnalysisRunData]] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    getAnalysisRuns(analysisId, analysisName, analysisVersion, includeResults)
  }

  override def getAnalysisRun(analysisName: String, analysisVersion: String, runUid: String, includeResults: Boolean = false): Try[AnalysisRunData] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    val results = if(includeResults){
      getRunResultsAsJSON(runUid).get
    } else Set.empty[AnalysisResultData]

    val queryF = db
      .run(analysisRunsTable.filter(r => r.parentID === analysisId && r.uid === runUid).take(1).result)
      .map(r => r.map(run => run.toAnalysisRunData(analysisName, analysisVersion, getInputsForRun(run.id), results)))(db.ioExecutionContext)

    Await.result(queryF, simpleQueryTimeout).head
  }

  private def getRunRepr(runUid: String): SoftwareAnalysisRunRepr = {
    val queryF = db.run(analysisRunsTable.filter(r => r.uid === runUid).take(1).result)

    Await.result(queryF, simpleQueryTimeout).headOption match {
      case Some(result) => result
      case None => throw new IllegalStateException(s"Analysis Run with uid $runUid not present in db")
    }

  }

  private def setRunState(runId: Long, runState: Int): Unit = {
    val action = db.run(analysisRunsTable.filter(r => r.id === runId).map(r => r.state).update(runState))
    Await.ready(action, simpleQueryTimeout)
  }

  override def setRunState(runUid: String, state: RunState, runInputIdsOpt: Option[Set[String]]): Try[Unit] = Try {
    val runRepr = getRunRepr(runUid)
    setRunState(runRepr.id, state.id)

    if(runInputIdsOpt.isDefined){
      // Connect inputs to run
      val inputEntityIds = Await.result(db.run(entitiesTable.filter(e => e.qualifier inSet runInputIdsOpt.get).map(_.id).result), simpleQueryTimeout)

      val inputInsertAction = db.run(DBIO.sequence(inputEntityIds.map(id => analysisRunInputsTable += AnalysisRunInput(-1, runRepr.id, id))))
      Await.ready(inputInsertAction, simpleQueryTimeout)
    }
  }

  override def setRunResults(runUid: String, timeStamp: LocalDateTime, logs: Array[String], results: Set[AnalysisResultData])(implicit serializer: JsonWriter[Object]): Try[Unit] = Try {
    val runRepr: SoftwareAnalysisRunRepr = getRunRepr(runUid)
    setRunState(runRepr.id, RunState.Finished.id)

    val newTimeStamp = timeStamp.format(DateTimeFormatter.ISO_DATE_TIME)
    val logsString = logs.mkString(";;;")

    val updateTimeStampAction = db.run(analysisRunsTable.filter(r => r.uid === runUid).map(r => r.timestamp).update(newTimeStamp))
    val updateLogsAction = db.run(analysisRunsTable.filter(r => r.uid === runUid).map(r => r.logs).update(logsString))

    Await.ready(updateTimeStampAction, simpleQueryTimeout)
    Await.ready(updateLogsAction, simpleQueryTimeout)

    results.foreach{ runResult =>
      // Serialize result content
      val content = serializer.write(runResult.content).compactPrint
      val resultDbObj = AnalysisResult(-1, runResult.uid, runRepr.id, runResult.isRevoked, content)

      // Write result entry
      val resultDbId = Await.result(db.run(idReturningResultTable += resultDbObj), simpleQueryTimeout)

      // Connect to affected entities
      runResult.affectedEntities.map(e => getEntityRepr(e.uid, e.kind).get.id).foreach { affectedEntityId =>
        Await.ready(db.run(resultValiditiesTable += AnalysisResultValidity(-1, resultDbId, affectedEntityId)), simpleQueryTimeout)
      }


    }
  }

  override def getRunResultsAsJSON(runUid: String, skip: Int = 0, limit: Int = 100): Try[Set[AnalysisResultData]] = Try {
    val runRepr: SoftwareAnalysisRunRepr = getRunRepr(runUid)

    val resQuery = db.run(analysisResultsTable.filter(r => r.runID === runRepr.id).sortBy(_.id).drop(skip).take(limit).result)
    val allResults = Await.result(resQuery, longActionTimeout)


    val resultEntitiesF = db.run {
      val join = for {(resultValidity, entity) <- resultValiditiesTable join entitiesTable on (_.entityId === _.id)}
        yield (resultValidity.resultId, entity)
      join.filter(t => t._1 inSet allResults.map(_.id).toSet).result
    }

    val resultToInputEntitiesMapping = Await.result(resultEntitiesF, longActionTimeout)

    allResults.map{ resultRep =>
      val allAssociatedEntities = resultToInputEntitiesMapping
        .filter(t => t._1 == resultRep.id)
        .map(t => toGenericEntityData(t._2))

      AnalysisResultData(resultRep.uid, resultRep.isRevoked, resultRep.jsonContent, allAssociatedEntities.toSet)
    }.toSet

  }




  override def storeEmptyAnalysisRun(analysisName: String, analysisVersion: String, runConfig: String): Try[String] = Try {
    val analysisId = getAnalysisRepr(analysisName, analysisVersion).id

    // Insert run
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME)
    val runRepr = SoftwareAnalysisRunRepr(-1, getFreshRunUuid(), runConfig, RunState.Created.id, isRevoked = false, analysisId, "", timestamp)
    Await.ready(db.run(idReturningAnalysisRunTable += runRepr), simpleQueryTimeout)

    runRepr.uid
  }

  override def getFreshResultUuids(noOfUuids: Int): Set[String] = {

    var uuids = Range(0, noOfUuids).map(_ => UUID.randomUUID().toString).toSet

    def newUuids(): Unit = uuids = Range(0, noOfUuids).map(_ => UUID.randomUUID().toString).toSet

    val query = analysisResultsTable.filter( r => r.uid inSet uuids).exists

    var uuidsFoundInDB = Await.result(db.run(query.result), simpleQueryTimeout)

    while(uuids.size < noOfUuids || uuidsFoundInDB){
      newUuids()
      uuidsFoundInDB = Await.result(db.run(query.result), simpleQueryTimeout)
    }

    uuids
  }

  override def getFreshRunUuid(): String = {
    var uuid = UUID.randomUUID().toString

    def exists: Boolean = Await.result(db.run(analysisRunsTable.filter(r => r.uid === uuid).exists.result), simpleQueryTimeout)

    while(exists) { uuid = UUID.randomUUID().toString }

    uuid
  }

  override def getJSONResultsFor(entityName: String, analysisFilter: Option[(String, String)], limit: Int, skip: Int): Try[Set[AnalysisResultData]] = Try {

    val eid = getEntityId(entityName)

    // Get all results associated with this entity
    val entityResultsF = db.run{
      val join = for { (_, result) <- resultValiditiesTable.filter(v => v.entityId === eid) join analysisResultsTable on (_.resultId === _.id)}
        yield result
      join.sortBy(_.id).drop(skip).take(limit).result
    }

    var allEntityResults = Await.result(entityResultsF, longActionTimeout)

    // Apply analysis filter
    if(analysisFilter.isDefined) {
      val analysisRepr = getAnalysisRepr(analysisFilter.get._1, analysisFilter.get._2)

      allEntityResults = allEntityResults.filter{ result =>
        val analysisId = Await.result(db.run{ analysisRunsTable.filter(run => run.id === result.runId).map(run => run.parentID).take(1).result }, simpleQueryTimeout).head

        analysisId == analysisRepr.id
      }
    }



    // Results may be associated with more than one entity, therefore: Collect mapping of results to entities for all results
    val resultEntitiesF = db.run {
      val join = for {(resultValidity, entity) <- resultValiditiesTable.filter(v => v.resultId inSet allEntityResults.map(_.id)) join entitiesTable on (_.entityId === _.id)}
        yield (resultValidity.resultId, entity)
      join.result
    }

    val resultToInputEntitiesMapping = Await.result(resultEntitiesF, longActionTimeout)

    // Build results
    allEntityResults.map{ resultRep =>
      val allAssociatedEntities = resultToInputEntitiesMapping
        .filter(t => t._1 == resultRep.id)
        .map(t => toGenericEntityData(t._2))

      AnalysisResultData(resultRep.uid, resultRep.isRevoked, resultRep.jsonContent, allAssociatedEntities.toSet)
    }.toSet

  }

  override def getEntityChildren(uid: String, skip: Int, limit:Int): Try[Seq[SoftwareEntityData]] = Try {
    val parentEntityId = getEntityId(uid)

    val queryF = db.run(entitiesTable.filter(swe => swe.parentID === parentEntityId).sortBy(_.id).drop(skip).take(limit).result)

    val entityRepResult = Await.result(queryF, longActionTimeout)

    buildEntities(entityRepResult)
  }

  private def getEntityId(qualifier: String): Long = {
    val queryF = db.run(entitiesTable.filter(swe => swe.qualifier === qualifier).take(1).map(_.id).result)

    Await.result(queryF, simpleQueryTimeout).head
  }

  private def getUidForEntityId(id: Long): String = {
    val queryF = db.run(entitiesTable.filter(e => e.id ===id).map(e => e.qualifier).take(1).result)

    Await.result(queryF, simpleQueryTimeout).head
  }

  private def toGenericEntityData(repr: SoftwareEntityRepr): GenericEntityData = {

    idToIdentifierCache.pushValue(repr.id, repr.fqn)

    val parentIdent = repr.parentId.map(parentId => idToIdentifierCache.getWithCache(parentId, () => getUidForEntityId(parentId)))

    new GenericEntityData(repr.name, repr.language, SoftwareEntityKind.fromId(repr.kindId),
      repr.repository, repr.hexHash.map(fromHex), repr.fqn, parentIdent)
  }


  override def getEntities(limit: Int, skip: Int, kindFilter: Option[SoftwareEntityKind], parentFilter: Option[String]): Try[Seq[GenericEntityData]] = Try {

    val queryF = if(parentFilter.isDefined) buildEntityQueryWithParentFilter(limit, skip, parentFilter.get, kindFilter)
      else if(kindFilter.isDefined) db.run(entitiesTable.filter(e => e.kind === kindFilter.get.id).sortBy(_.id).drop(skip).take(limit).result)
      else db.run(entitiesTable.sortBy(_.id).drop(skip).take(limit).result)

    Await.result(queryF, longActionTimeout).map(toGenericEntityData)

  }

  private def buildEntityQueryWithParentFilter(limit: Int, skip: Int, parentUid: String, kindFilter: Option[SoftwareEntityKind]) = {

    val joinQuery = if(kindFilter.isEmpty){
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


  implicit private def tryToBool: Try[Boolean] => Boolean = {
    case Success(value) => value
    case Failure(ex) =>
      log.error("Failed to perform DB lookup", ex)
      false

  }

  override def hasAnalysis(analysisName: String): Boolean = Try {
    val queryF = db.run(analysesTable.filter(a => a.name === analysisName).exists.result)
    Await.result(queryF, simpleQueryTimeout)
  }

  override def hasAnalysisRun(analysisName: String, analysisVersion: String, runUid: String): Boolean = Try {
    val requiredId = getAnalysisRepr(analysisName, analysisVersion).id
    val queryF = db.run(analysisRunsTable.filter(r => r.uid === runUid && r.parentID === requiredId).exists.result)

    Await.result(queryF, simpleQueryTimeout)
  }

  override def hasEntity(ident: String): Boolean = Try {
    val queryF = db.run(entitiesTable.filter( e => e.qualifier === ident).exists.result)
    Await.result(queryF, simpleQueryTimeout)
  }
}
