package de.tudo.sse.spareuse.core.storage

import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}

import slick.jdbc.PostgresProfile.api._

package object postgresql {

  class SoftwareEntities(tag: Tag) extends Table[(Long, String, String, String, Int, String, Option[Long])](tag, "entities") {

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME")

    def qualifier: Rep[String] = column[String]("FQ", O.Unique)

    def language: Rep[String] = column[String]("LANG")

    def kind: Rep[Int] = column[Int]("KIND")

    def repository: Rep[String] = column[String]("REPO")

    def parentID: Rep[Option[Long]] = column[Option[Long]]("PARENT_ID")

    override def * : ProvenShape[(Long, String, String, String, Int, String, Option[Long])] =
      (id, name, qualifier, language, kind, repository, parentID)

    def parent: ForeignKeyQuery[SoftwareEntities, (Long, String, String, String, Int, String, Option[Long])] =
      foreignKey("PARENT_FK", parentID, TableQuery[SoftwareEntities])(_.id.?)
  }

  type SoftwareEntityRepr = (Long, String, String, String, Int, String, Option[Long])

}
