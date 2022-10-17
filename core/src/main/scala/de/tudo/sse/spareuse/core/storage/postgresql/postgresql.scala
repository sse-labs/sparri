package de.tudo.sse.spareuse.core.storage

import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}

import slick.jdbc.PostgresProfile.api._

package object postgresql {

  class SoftwareEntities(tag: Tag) extends Table[(Long, String, String, String, Int, String, Option[Long], Option[String])](tag, "entities") {

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME")

    def qualifier: Rep[String] = column[String]("FQ", O.Unique)

    def language: Rep[String] = column[String]("LANG")

    def kind: Rep[Int] = column[Int]("KIND")

    def repository: Rep[String] = column[String]("REPO")

    def parentID: Rep[Option[Long]] = column[Option[Long]]("PARENT_ID")

    def hash: Rep[Option[String]] = column[Option[String]]("HASH")

    override def * : ProvenShape[(Long, String, String, String, Int, String, Option[Long], Option[String])] =
      (id, name, qualifier, language, kind, repository, parentID, hash)

    def parent: ForeignKeyQuery[SoftwareEntities, (Long, String, String, String, Int, String, Option[Long], Option[String])] =
      foreignKey("PARENT_FK", parentID, TableQuery[SoftwareEntities])(_.id.?)
  }

  type SoftwareEntityRepr = (Long, String, String, String, Int, String, Option[Long], Option[String])

}
