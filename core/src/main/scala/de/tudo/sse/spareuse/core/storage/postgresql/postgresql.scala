package de.tudo.sse.spareuse.core.storage

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaProgram
import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}
import slick.jdbc.PostgresProfile.api._


package object postgresql {

  type SoftwareEntityRepr = (Long, String, String, String, Int, String, Option[Long], Option[String])

  class SoftwareEntities(tag: Tag) extends Table[SoftwareEntityRepr](tag, "entities") {

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME")

    def qualifier: Rep[String] = column[String]("FQ", O.Unique)

    def language: Rep[String] = column[String]("LANG")

    def kind: Rep[Int] = column[Int]("KIND")

    def repository: Rep[String] = column[String]("REPO")

    def parentID: Rep[Option[Long]] = column[Option[Long]]("PARENT_ID")

    def hash: Rep[Option[String]] = column[Option[String]]("HASH")

    override def * : ProvenShape[SoftwareEntityRepr] =
      (id, name, qualifier, language, kind, repository, parentID, hash)

    def parent: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("PARENT_FK", parentID, TableQuery[SoftwareEntities])(_.id.?)
  }


  type SoftwareAnalysisRepr = (Long, String, String, String, String, String, String, Boolean, Int)
  class SoftwareAnalyses(tag: Tag) extends Table[SoftwareAnalysisRepr](tag, "analyses") {

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME")

    def version: Rep[String] = column[String]("VERSION")

    def description: Rep[String] = column[String]("DESCRIPTION")

    def builtOn: Rep[String] = column[String]("BUILT_ON")

    def registeredBy: Rep[String] = column[String]("REGISTERED_BY")

    def inputLanguages: Rep[String] = column[String]("LANGUAGES")

    def isRevoked: Rep[Boolean] = column[Boolean]("REVOKED")

    //TODO: Reference to result format

    def inputKind: Rep[Int] = column[Int]("INPUT_KIND")

    override def * : ProvenShape[SoftwareAnalysisRepr] =
      (id, name, version, description, builtOn, registeredBy, inputLanguages, isRevoked, inputKind)
  }

  case class SoftwareAnalysisRunRepr(id: Long, config: String, isRevoked: Boolean, parentId: Long)

  class SoftwareAnalysisRuns(tag: Tag) extends Table[SoftwareAnalysisRunRepr](tag, "analysisruns"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    //TODO: Timestamp
    //TODO: Logs

    def configuration: Rep[String] = column[String]("CONFIGURATION")

    def isRevoked: Rep[Boolean] = column[Boolean]("REVOKED")

    def parentID: Rep[Long] = column[Long]("ANALYSIS_ID")


    override def * : ProvenShape[SoftwareAnalysisRunRepr] =
      (id, configuration, isRevoked, parentID) <> ((SoftwareAnalysisRunRepr.apply _).tupled, SoftwareAnalysisRunRepr.unapply)

    def parent: ForeignKeyQuery[SoftwareAnalyses, SoftwareAnalysisRepr] =
      foreignKey("ANALYSIS_FK", parentID, TableQuery[SoftwareAnalyses])(_.id)


  }

  case class AnalysisRunInput(id: Long, analysisRunId: Long, inputEntityId: Long)
  class AnalysisRunInputs(tag: Tag) extends Table[AnalysisRunInput](tag, "anlysisruninputs"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def analysisRunID: Rep[Long] = column[Long]("ANALYSIS_ID")

    def inputEntityID: Rep[Long] = column[Long]("INPUT_ID")

    override def * : ProvenShape[AnalysisRunInput] =
      (id, analysisRunID, inputEntityID) <> ((AnalysisRunInput.apply _).tupled, AnalysisRunInput.unapply)

  }

}
