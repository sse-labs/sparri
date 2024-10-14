package org.anon.spareuse.core.storage.postgresql

import slick.lifted.{ForeignKeyQuery, ProvenShape, Tag}
import slick.jdbc.PostgresProfile.api._

object JavaDefinitions {

  case class JavaTypeName(id: Long, name: String)

  class JavaTypeNames(tag: Tag) extends Table[JavaTypeName](tag, "javatypename"){
    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def name: Rep[String] = column[String]("NAME", O.Unique)

    override def * : ProvenShape[JavaTypeName] = (id, name) <> ((JavaTypeName.apply _).tupled, JavaTypeName.unapply)
  }

  case class JavaMethodDescriptor(id: Long, descriptor: String)

  class JavaMethodDescriptors(tag: Tag) extends Table[JavaMethodDescriptor](tag, "javadescriptors"){
    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def descriptor: Rep[String] = column[String]("NAME", O.Unique)

    override def * : ProvenShape[JavaMethodDescriptor] = (id, descriptor) <>
      ((JavaMethodDescriptor.apply _).tupled, JavaMethodDescriptor.unapply)
  }

  type JavaProgramRepr = (Long, String)

  class JavaPrograms(tag: Tag) extends Table[JavaProgramRepr](tag, "javaprograms"){
    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def publicationDate: Rep[String] = column[String]("PUB_DATE")

    override def * : ProvenShape[JavaProgramRepr] = (id, publicationDate)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)
  }

  case class JavaClassInterface(id: Long, classId: Long, interfaceId: Long)

  class JavaClassInterfaces(tag: Tag) extends Table[JavaClassInterface](tag, "javaclassinterfaces"){
    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def classId: Rep[Long] = column[Long]("CLASS")

    def interfaceId: Rep[Long] = column[Long]("INTERFACE")

    override def * : ProvenShape[JavaClassInterface] = (id, classId, interfaceId) <>
      ((JavaClassInterface.apply _).tupled, JavaClassInterface.unapply)

    def javaClass: ForeignKeyQuery[JavaClasses, JavaClassRepr] =
      foreignKey("CLASS", classId, TableQuery[JavaClasses])(_.id)

    def interfaceName: ForeignKeyQuery[JavaTypeNames, JavaTypeName] =
      foreignKey("INTERFACE", interfaceId, TableQuery[JavaTypeNames])(_.id)
  }

  case class JavaClassRepr(id: Long, typeNameId: Long, superTypeNameId: Option[Long], isInterface: Boolean, isFinal: Boolean, isAbstract: Boolean)

  class JavaClasses(tag: Tag) extends Table[JavaClassRepr](tag, "javaclasses"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def thisType: Rep[Long] = column[Long]("FQN")

    def superType: Rep[Option[Long]] = column[Option[Long]]("SUPERTYPE_FQN")

    def isInterface: Rep[Boolean] = column[Boolean]("IS_INTERFACE")

    def isFinal: Rep[Boolean] = column[Boolean]("IS_FINAL")

    def isAbstract: Rep[Boolean] = column[Boolean]("IS_ABSTRACT")

    override def * : ProvenShape[JavaClassRepr] = (id, thisType, superType, isInterface, isFinal, isAbstract) <>
      ((JavaClassRepr.apply _ ).tupled, JavaClassRepr.unapply)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)

    def typeName: ForeignKeyQuery[JavaTypeNames, JavaTypeName] =
      foreignKey("FQN", thisType, TableQuery[JavaTypeNames])(_.id)

    def superTypeName: ForeignKeyQuery[JavaTypeNames, JavaTypeName] =
      foreignKey("SUPERTYPE_FQN", superType, TableQuery[JavaTypeNames])(_.id.?)
  }

  case class JavaMethodRepr(id: Long, descriptorId: Long, isFinal: Boolean, isStatic: Boolean, isAbstract: Boolean, visibility: String, hash: Int)
  class JavaMethods(tag: Tag) extends Table[JavaMethodRepr](tag, "javamethods"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def descriptor: Rep[Long] = column[Long]("DESCRIPTOR")

    def isFinal: Rep[Boolean] = column[Boolean]("IS_FINAL")

    def isStatic: Rep[Boolean] = column[Boolean]("IS_STATIC")

    def isAbstract: Rep[Boolean] = column[Boolean]("IS_ABSTRACT")

    def visibility: Rep[String] = column[String]("VISIBILITY")

    def methodHash: Rep[Int] = column[Int]("HASH")

    override def * : ProvenShape[JavaMethodRepr] = (id, descriptor, isFinal, isStatic, isAbstract, visibility, methodHash) <>
      ((JavaMethodRepr.apply _).tupled, JavaMethodRepr.unapply)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)

    def descriptorValue: ForeignKeyQuery[JavaMethodDescriptors, JavaMethodDescriptor] =
      foreignKey("DESCRIPTOR", descriptor, TableQuery[JavaMethodDescriptors])(_.id)
  }
  case class JavaInvocationRepr(id: Long, declTypeNameId: Long, descriptorId: Long, kindId: Int, pc: Int)

  class JavaInvocationStatements(tag: Tag) extends Table[JavaInvocationRepr](tag, "javainvocations"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def declaredType: Rep[Long] = column[Long]("DECLARED_TYPE")

    def descriptor: Rep[Long] = column[Long]("DESCRIPTOR")

    def kind: Rep[Int] = column[Int]("KIND")

    def pc: Rep[Int] = column[Int]("PC")

    override def * : ProvenShape[JavaInvocationRepr] = (id, declaredType, descriptor, kind, pc) <>
      ((JavaInvocationRepr.apply _).tupled, JavaInvocationRepr.unapply)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)

    def declaredTypeName: ForeignKeyQuery[JavaTypeNames, JavaTypeName] =
      foreignKey("DECLARED_TYPE", declaredType, TableQuery[JavaTypeNames])(_.id)

    def descriptorValue: ForeignKeyQuery[JavaMethodDescriptors, JavaMethodDescriptor] =
      foreignKey("DESCRIPTOR", descriptor, TableQuery[JavaMethodDescriptors])(_.id)
  }

  case class JavaFieldAccessRepr(id: Long, fieldTypeNameId: Long, declTypeNameId: Long, kindId: Int, pc: Int)

  class JavaFieldAccessStatements(tag: Tag) extends Table[JavaFieldAccessRepr](tag, "javafieldaccesses"){

    def id: Rep[Long] = column[Long]("ID", O.PrimaryKey)

    def fieldType: Rep[Long] = column[Long]("FIELD_TYPE")

    def declaredType: Rep[Long] = column[Long]("DECLARED_TYPE")

    def kind: Rep[Int] = column[Int]("KIND")

    def pc: Rep[Int] = column[Int]("PC")

    def * : ProvenShape[JavaFieldAccessRepr] = (id, fieldType, declaredType, kind, pc) <>
      ((JavaFieldAccessRepr.apply _).tupled, JavaFieldAccessRepr.unapply)

    def entity: ForeignKeyQuery[SoftwareEntities, SoftwareEntityRepr] =
      foreignKey("ID", id, TableQuery[SoftwareEntities])(_.id)

    def fieldTypeName: ForeignKeyQuery[JavaTypeNames, JavaTypeName] =
      foreignKey("FIELD_TYPE", fieldType, TableQuery[JavaTypeNames])(_.id)

    def declaredTypeName: ForeignKeyQuery[JavaTypeNames, JavaTypeName] =
      foreignKey("DECLARED_TYPE", declaredType, TableQuery[JavaTypeNames])(_.id)

  }

}
