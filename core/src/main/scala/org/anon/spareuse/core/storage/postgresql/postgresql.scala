package org.anon.spareuse.core.storage

import org.anon.spareuse.core.formats.{AnalysisResultFormat, EmptyFormat}
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, RunState, SoftwareEntityKind}
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.storage.postgresql.ResultType.{BaseResult, GraphResult, ListResult, MapResult, ObjectResult}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}
import slick.jdbc.PostgresProfile.api._

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter


package object postgresql {

  case class SoftwareEntityRepr(id: Long, name: String, identifier: String, language: String, kindId: Int,
                                repository: String, parentId: Option[Long], hexHash: Option[String])

  class SoftwareEntities(tag: Tag) extends Table[SoftwareEntityRepr](tag, "entities") {

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME")

    def identifier: Rep[String] = column[String]("IDENTIFIER")

    def language: Rep[String] = column[String]("LANG")

    def kind: Rep[Int] = column[Int]("KIND")

    def repository: Rep[String] = column[String]("REPO")

    def parentID: Rep[Option[Long]] = column[Option[Long]]("PARENT_ID")

    def hash: Rep[Option[String]] = column[Option[String]]("HASH")

    override def * : ProvenShape[SoftwareEntityRepr] =
      (id, name, identifier, language, kind, repository, parentID, hash)<> ((SoftwareEntityRepr.apply _).tupled, SoftwareEntityRepr.unapply)

    def parent: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("PARENT_FK", parentID, TableQuery[SoftwareEntities])(_.id.?)

    def idx = index("unique_ident", (parentID, identifier), unique = true)
  }


  case class SoftwareAnalysisRepr(id: Long, name: String, version: String, description: String, builtOn: String,
                                  registeredBy: String, inputLanguages: String, formatId: Long, revoked: Boolean,
                                  inputKind: Int, batchProcessing: Boolean, isIncremental: Boolean){
    def toAnalysisData(format: AnalysisResultFormat, executions: Set[AnalysisRunData] = Set.empty): AnalysisData = {
      AnalysisData(name, version, description, builtOn, registeredBy, inputLanguages.split(",").toSet, revoked,
        format, SoftwareEntityKind.fromId(inputKind), batchProcessing, isIncremental, executions)
    }
  }

  def toAnalysisRepr(data: AnalysisData, formatId: Long): SoftwareAnalysisRepr = {
    SoftwareAnalysisRepr(-1, data.name, data.version, data.description, data.builtOn, data.registeredBy,
      data.inputLanguages.mkString(","), formatId, data.isRevoked, data.inputKind.id, data.doesBatchProcessing, data.isIncremental)
  }

  class SoftwareAnalyses(tag: Tag) extends Table[SoftwareAnalysisRepr](tag, "analyses") {

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME")

    def version: Rep[String] = column[String]("VERSION")

    def description: Rep[String] = column[String]("DESCRIPTION")

    def builtOn: Rep[String] = column[String]("BUILT_ON")

    def registeredBy: Rep[String] = column[String]("REGISTERED_BY")

    def inputLanguages: Rep[String] = column[String]("LANGUAGES")

    def isRevoked: Rep[Boolean] = column[Boolean]("REVOKED")

    def formatId: Rep[Long] = column[Long]("RESULT_FORMAT")

    def inputKind: Rep[Int] = column[Int]("INPUT_KIND")

    def batchProcessing: Rep[Boolean] = column[Boolean]("IS_BATCH")

    def isIncremental: Rep[Boolean] = column[Boolean]("IS_INCREMENTAL")

    override def * : ProvenShape[SoftwareAnalysisRepr] =
      (id, name, version, description, builtOn, registeredBy, inputLanguages, formatId, isRevoked, inputKind, batchProcessing,
        isIncremental) <> ((SoftwareAnalysisRepr.apply _).tupled, SoftwareAnalysisRepr.unapply)

    def format: ForeignKeyQuery[ResultFormats, ResultFormat] =
      foreignKey("FORMAT_FK", formatId, TableQuery[ResultFormats])(_.id)
  }

  case class SoftwareAnalysisRunRepr(id: Long, uid:String, config: String, state: Int, isRevoked: Boolean, parentId: Long, logs: String, timestamp: String, duration: Long){

    def toAnalysisRunData(analysisName: String, analysisVersion: String, inputs: Set[SoftwareEntityData] = Set.empty,
                          results: Set[AnalysisResultData] = Set.empty): AnalysisRunData = {
      val rState = RunState(state)
      AnalysisRunData(uid, LocalDateTime.from(DateTimeFormatter.ISO_DATE_TIME.parse(timestamp)), duration, logs.split(";;;"),
        config, rState, isRevoked, inputs, results, analysisName, analysisVersion)
    }

  }

  class SoftwareAnalysisRuns(tag: Tag) extends Table[SoftwareAnalysisRunRepr](tag, "analysisruns"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def uid: Rep[String] = column[String]("UID", O.Unique)

    def configuration: Rep[String] = column[String]("CONFIGURATION")

    def state: Rep[Int] = column[Int]("STATE")

    def isRevoked: Rep[Boolean] = column[Boolean]("REVOKED")

    def parentID: Rep[Long] = column[Long]("ANALYSIS_ID")

    def logs: Rep[String] = column[String]("LOGS")

    def timestamp: Rep[String] = column[String]("TIMESTAMP")

    def duration: Rep[Long] = column[Long]("DURATION")


    override def * : ProvenShape[SoftwareAnalysisRunRepr] =
      (id, uid, configuration, state, isRevoked, parentID, logs, timestamp, duration) <>
        ((SoftwareAnalysisRunRepr.apply _).tupled, SoftwareAnalysisRunRepr.unapply)

    def parent: ForeignKeyQuery[SoftwareAnalyses, SoftwareAnalysisRepr] =
      foreignKey("ANALYSIS_FK", parentID, TableQuery[SoftwareAnalyses])(_.id)


  }

  case class AnalysisRunInput(id: Long, analysisRunId: Long, inputEntityId: Long)
  class AnalysisRunInputs(tag: Tag) extends Table[AnalysisRunInput](tag, "anlysisruninputs"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def analysisRunID: Rep[Long] = column[Long]("ANALYSIS_RUN_ID")

    def inputEntityID: Rep[Long] = column[Long]("INPUT_ENTITY_ID")

    override def * : ProvenShape[AnalysisRunInput] =
      (id, analysisRunID, inputEntityID) <> ((AnalysisRunInput.apply _).tupled, AnalysisRunInput.unapply)

    def analysisRun: ForeignKeyQuery[SoftwareAnalysisRuns, SoftwareAnalysisRunRepr] =
      foreignKey("RUN_FK", analysisRunID, TableQuery[SoftwareAnalysisRuns])(_.id)

    def inputEntity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("INPUT_FK", inputEntityID, TableQuery[SoftwareEntities])(_.id)

  }

  case class AnalysisRunResultRelation(id: Long, analysisRunId: Long, analysisResultId: Long)

  class AnalysisRunResultRelations(tag: Tag) extends Table[AnalysisRunResultRelation](tag, "analysisrunresults"){
    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def analysisRunID: Rep[Long] = column[Long]("ANALYSIS_RUN_ID")

    def resultID: Rep[Long] = column[Long]("RESULT_ID")

    override def * : ProvenShape[AnalysisRunResultRelation] =
      (id, analysisRunID, resultID) <> ((AnalysisRunResultRelation.apply _ ).tupled, AnalysisRunResultRelation.unapply)

    def analysisRun: ForeignKeyQuery[SoftwareAnalysisRuns, SoftwareAnalysisRunRepr] =
      foreignKey("RUN_FK", analysisRunID, TableQuery[SoftwareAnalysisRuns])(_.id)

    def result: ForeignKeyQuery[AnalysisResults, AnalysisResult] =
      foreignKey("RESULT_FK", resultID, TableQuery[AnalysisResults])(_.id)
  }

  case class AnalysisResult(id: Long, uid: String, runId: Long, isRevoked: Boolean, jsonContent: String)

  class AnalysisResults(tag: Tag) extends Table[AnalysisResult](tag, "analysisresults"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def uid: Rep[String] = column[String]("UID", O.Unique)

    def runID: Rep[Long] = column[Long]("RUN_ID")

    def isRevoked: Rep[Boolean] = column[Boolean]("IS_REVOKED")

    def content: Rep[String] = column[String]("CONTENT")

    override def * : ProvenShape[AnalysisResult] =
      (id, uid, runID, isRevoked, content) <> ((AnalysisResult.apply _ ).tupled, AnalysisResult.unapply)

    def run: ForeignKeyQuery[SoftwareAnalysisRuns, SoftwareAnalysisRunRepr] =
      foreignKey("RUN_FK",  runID, TableQuery[SoftwareAnalysisRuns])(_.id)


  }

  case class AnalysisResultValidity(id: Long, analysisResultId: Long, entityId: Long)

  class AnalysisResultValidities(tag: Tag) extends Table[AnalysisResultValidity](tag, "resultentities"){
    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def resultId: Rep[Long] = column[Long]("ANALYSIS_RESULT_ID")

    def entityId: Rep[Long] = column[Long]("ENTITY_ID")

    override def * : ProvenShape[AnalysisResultValidity] =
      (id, resultId, entityId) <> ((AnalysisResultValidity.apply _).tupled, AnalysisResultValidity.unapply)

    def result: ForeignKeyQuery[AnalysisResults, AnalysisResult] =
      foreignKey("RESULT_FK", resultId, TableQuery[AnalysisResults])(_.id)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ENTITY_FK", entityId, TableQuery[SoftwareEntities])(_.id)

  }



  case class ResultFormat(id: Long, identifier: String, resultType: Int)

  object ResultFormatPredef {
    val StringFormat: ResultFormat = ResultFormat(1, "STRING", BaseResult.id)
    val NumberFormat: ResultFormat = ResultFormat(2, "NUMBER", BaseResult.id)
    val EntityRefFormat: ResultFormat = ResultFormat(3, "ENTITY_REFERENCE", BaseResult.id)
    val EmptyFormat: ResultFormat = ResultFormat(4, "EMPTY", BaseResult.id)

    val allPredefFormats: Seq[ResultFormat] = Seq(StringFormat, NumberFormat, EntityRefFormat, EmptyFormat)
  }

  final object ResultType extends Enumeration {

    type ResultType = Value

    final val baseTypeId = 1
    final val mapTypeId = 2
    final val listTypeId = 3
    final val objectTypeId = 4
    final val graphTypeId = 5
    final val namedPropTypeId = 6

    val BaseResult: ResultType = Value(baseTypeId)
    val MapResult: ResultType = Value(mapTypeId)
    val ListResult: ResultType = Value(listTypeId)
    val ObjectResult: ResultType = Value(objectTypeId)
    val GraphResult: ResultType = Value(graphTypeId)
    val NamedPropertyResult: ResultType = Value(namedPropTypeId)
  }

  class ResultFormats(tag: Tag) extends Table[ResultFormat](tag, "resultformats"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def identifier: Rep[String] = column[String]("IDENTIFIER")

    def resultType: Rep[Int] = column[Int]("RESULT_TYPE")

    override def * : ProvenShape[ResultFormat] =
      (id, identifier, resultType) <> ((ResultFormat.apply _).tupled, ResultFormat.unapply)
  }

  case class NestedResultFormatReference(originId: Long, targetId: Long, nestingKind: Int, description: String)

  object ResultNestingKind extends Enumeration {

    type ResultNestingKind = Value

    val ObjectProperty: Value = Value(0)
    val MapKey: Value = Value(1)
    val MapValue: Value = Value(2)
    val ListElement: Value = Value(3)
    val NamedProperty: Value = Value(4)
    val NodeProperty: Value = Value(5)
    val EdgeProperty: Value = Value(6)
  }

  class NestedResultFormatReferences(tag: Tag) extends Table[NestedResultFormatReference](tag, "nestedresults"){

    def originId: Rep[Long] = column[Long]("ORIGIN")

    def targetId: Rep[Long] = column[Long]("TARGET")

    def nestingKind: Rep[Int] = column[Int]("NESTING_KIND")

    def description: Rep[String] = column[String]("DESCRIPTION")

    override def * : ProvenShape[NestedResultFormatReference] =
      (originId, targetId, nestingKind, description) <> ((NestedResultFormatReference.apply _).tupled, NestedResultFormatReference.unapply)

    def origin: ForeignKeyQuery[ResultFormats, ResultFormat] =
      foreignKey("ORIGIN_FK", originId, TableQuery[ResultFormats])(_.id)

    def target: ForeignKeyQuery[ResultFormats, ResultFormat] =
      foreignKey("TARGET_FK", targetId, TableQuery[ResultFormats])(_.id)
  }

}
