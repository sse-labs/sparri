package de.tudo.sse.spareuse.core.storage.postgresql

import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}
import slick.jdbc.PostgresProfile.api._

object JavaDefinitions {

  type JavaClassRepr = (Long, String, Option[String], String, Boolean)

  class JavaClasses(tag: Tag) extends Table[JavaClassRepr](tag, "javaclasses"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def thisType: Rep[String] = column[String]("FQN")

    def superType: Rep[Option[String]] = column[Option[String]]("SUPERTYPE_FQN")

    def interfaceTypes: Rep[String] = column[String]("INTERFACES")

    def isInterface: Rep[Boolean] = column[Boolean]("IS_INTERFACE")

    override def * : ProvenShape[JavaClassRepr] = (id, thisType, superType, interfaceTypes, isInterface)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)
  }

  type JavaMethodRepr = (Long, String, Int, String, Boolean, Boolean, Boolean, String)

  class JavaMethods(tag: Tag) extends Table[JavaMethodRepr](tag, "javamethods"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def returnType: Rep[String] = column[String]("RETURN_TYPE")

    def paramCount: Rep[Int] = column[Int]("PARAMETER_CNT")

    def paramTypes: Rep[String] = column[String]("PARAMETER_TYPES")

    def isFinal: Rep[Boolean] = column[Boolean]("IS_FINAL")

    def isStatic: Rep[Boolean] = column[Boolean]("IS_STATIC")

    def isAbstract: Rep[Boolean] = column[Boolean]("IS_ABSTRACT")

    def visibility: Rep[String] = column[String]("VISIBILITY")


    override def * : ProvenShape[JavaMethodRepr] = (id, returnType, paramCount, paramTypes, isFinal, isStatic, isAbstract, visibility)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)
  }

  type JavaInvocationRepr = (Long, String, Int, String, Int, Int)

  class JavaInvocationStatements(tag: Tag) extends Table[JavaInvocationRepr](tag, "javainvocations"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def declaredType: Rep[String] = column[String]("DECLARED_TYPE")

    def parameterCount: Rep[Int] = column[Int]("PARAMETER_CNT")

    def returnType: Rep[String] = column[String]("RETURN_TYPE")

    def kind: Rep[Int] = column[Int]("KIND")

    def pc: Rep[Int] = column[Int]("PC")

    override def * : ProvenShape[JavaInvocationRepr] = (id, declaredType, parameterCount, returnType, kind, pc)

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
