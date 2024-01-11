package org.anon.spareuse.core.storage.postgresql

import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}
import slick.jdbc.PostgresProfile.api._

object JavaDefinitions {

  type JavaClassRepr = (Long, String, Option[String], String, Boolean, Boolean, Boolean)

  class JavaClasses(tag: Tag) extends Table[JavaClassRepr](tag, "javaclasses"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def thisType: Rep[String] = column[String]("FQN")

    def superType: Rep[Option[String]] = column[Option[String]]("SUPERTYPE_FQN")

    def interfaceTypes: Rep[String] = column[String]("INTERFACES")

    def isInterface: Rep[Boolean] = column[Boolean]("IS_INTERFACE")

    def isFinal: Rep[Boolean] = column[Boolean]("IS_FINAL")

    def isAbstract: Rep[Boolean] = column[Boolean]("IS_ABSTRACT")

    override def * : ProvenShape[JavaClassRepr] = (id, thisType, superType, interfaceTypes, isInterface, isFinal, isAbstract)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)
  }

  type JavaMethodRepr = (Long, String, Boolean, Boolean, Boolean, String, Int)

  class JavaMethods(tag: Tag) extends Table[JavaMethodRepr](tag, "javamethods"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def descriptor: Rep[String] = column[String]("DESCRIPTOR")

    def isFinal: Rep[Boolean] = column[Boolean]("IS_FINAL")

    def isStatic: Rep[Boolean] = column[Boolean]("IS_STATIC")

    def isAbstract: Rep[Boolean] = column[Boolean]("IS_ABSTRACT")

    def visibility: Rep[String] = column[String]("VISIBILITY")

    def methodHash: Rep[Int] = column[Int]("HASH")

    override def * : ProvenShape[JavaMethodRepr] = (id, descriptor, isFinal, isStatic, isAbstract, visibility, methodHash)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)
  }

  type JavaInvocationRepr = (Long, String, String, Int, Int)

  class JavaInvocationStatements(tag: Tag) extends Table[JavaInvocationRepr](tag, "javainvocations"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def declaredType: Rep[String] = column[String]("DECLARED_TYPE")

    def descriptor: Rep[String] = column[String]("DESCRIPTOR")

    def kind: Rep[Int] = column[Int]("KIND")

    def pc: Rep[Int] = column[Int]("PC")

    override def * : ProvenShape[JavaInvocationRepr] = (id, declaredType, descriptor, kind, pc)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)
  }

  type JavaFieldAccessRepr = (Long, String, String, Int, Int)

  class JavaFieldAccessStatements(tag: Tag) extends Table[JavaFieldAccessRepr](tag, "javafieldaccesses"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def fieldType: Rep[String] = column[String]("FIELD_TYPE")

    def declaredType: Rep[String] = column[String]("DECLARED_TYPE")

    def kind: Rep[Int] = column[Int]("KIND")

    def pc: Rep[Int] = column[Int]("PC")

    def * : ProvenShape[JavaFieldAccessRepr] = (id, fieldType, declaredType, kind, pc)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)

  }

}
