package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.JavaFieldAccessType.JavaFieldAccessType
import org.anon.spareuse.core.model.entities.JavaEntities.JavaInvocationType.JavaInvocationType
import org.opalj.br.MethodDescriptor

object JavaEntities {

  def gavToProgramIdent(gav: String): Option[String] = {
    if(gav.count(_ ==':') != 2) return None
    val ga = gav.substring(0, gav.lastIndexOf(":"))

    Some(s"$ga!$gav")
  }

  def buildLibrary(ga: String, repoIdent: String = "mvn"): JavaLibrary = new JavaLibrary(ga, repoIdent, -1L)

  def buildProgram(gav: String, repoIdent: String = "mvn", uploadTime: String, hash: Array[Byte] = Array.empty): JavaProgram = {
    buildProgramFor(buildLibrary(gav.substring(0, gav.lastIndexOf(":")), repoIdent), gav, uploadTime, hash)
  }

  def buildProgramFor(jl: JavaLibrary, gav: String, uploadTime: String, hash: Array[Byte] = Array.empty): JavaProgram = {
    val jp = new JavaProgram(gav, gav.substring(gav.lastIndexOf(":") + 1), -1L, jl.repository, uploadTime, hash)
    jp.setParent(jl)
    jp
  }

  def buildPackage(gav: String, packageName: String, repoIdent: String, uploadTime: String): JavaPackage = {
    buildPackageFor(buildProgram(gav, repoIdent, uploadTime), packageName)
  }

  def buildPackageFor(jp: JavaProgram, packageName: String): JavaPackage = {
    val jpa = new JavaPackage(packageName, -1L, jp.repository)
    jpa.setParent(jp)
    jpa
  }

  def buildClass(gav: String, packageName: String, className: String, fqn: String, isInterface:Boolean, isFinal:Boolean, isAbstract:Boolean, superTypeFqn: Option[String] = None, interfaceFqns: Set[String] = Set.empty, jarUploadTime: String = "<NONE>", repoIdent: String = "mvn", hash: Array[Byte] = Array.empty): JavaClass = {
    buildClassFor(buildPackage(gav, packageName, repoIdent, jarUploadTime), className, fqn, isInterface, isFinal, isAbstract, superTypeFqn, interfaceFqns, hash)
  }

  def buildClassFor(jp: JavaPackage, className: String, fqn: String, isInterface: Boolean, isFinal: Boolean, isAbstract: Boolean, superTypeFqn: Option[String] = None, interfaceFqns: Set[String] = Set.empty, hash: Array[Byte] = Array.empty): JavaClass = {
    val classObj = new JavaClass(className, fqn, -1L, superTypeFqn, interfaceFqns, isInterface, isFinal, isAbstract, jp.repository, hash)
    classObj.setParent(jp)
    classObj
  }

  def buildMethodFor(jc: JavaClass, methodName: String, descriptor: String, isFinal: Boolean, isStatic: Boolean, isAbstract: Boolean, visibility: String, hashCode: Int): JavaMethod = {
    val methodObj = new JavaMethod(methodName, descriptor, -1L, isFinal, isStatic, isAbstract, visibility, jc.repository, hashCode)
    methodObj.setParent(jc)
    methodObj
  }

  abstract class PathIdentifiableJavaEntity private[entities] (entityName: String,
                                                     entityIdent: String,
                                                     entityId: Long,
                                                     repositoryIdent: String,
                                                     hashedBytes: Option[Array[Byte]]) extends SoftwareEntityData {
    override val name: String = entityName
    override val language: String = "java"
    override val repository: String = repositoryIdent

    override val binaryHash: Option[Array[Byte]] = hashedBytes

    override val id: Long = entityId

    override val identifier: String = entityIdent

    lazy val uid: String = getParent.map {
      case pije: PathIdentifiableJavaEntity => pije.uid + "!" + identifier
      case _ => ""
    }.getOrElse(identifier)
  }

  class JavaLibrary(val libraryName: String,
                    repositoryIdent: String,
                    entityId: Long) extends PathIdentifiableJavaEntity(libraryName, libraryName, entityId, repositoryIdent, None){
    override val kind: SoftwareEntityKind = SoftwareEntityKind.Library

    def getPrograms: Set[JavaProgram] = getChildren.map(_.asInstanceOf[JavaProgram])
  }

  class JavaProgram(val programName: String,
                    val programVersion: String,
                    entityId: Long,
                    repositoryIdent: String,
                    uploadTime: String,
                    hashedBytes: Array[Byte]) extends PathIdentifiableJavaEntity(programName, programVersion, entityId, repositoryIdent, Some(hashedBytes)) {

    override val kind: SoftwareEntityKind = SoftwareEntityKind.Program

    private lazy val isGAV: Boolean = programName.count(_ == ':') == 2

    val ga: String = if(isGAV) programName.substring(0, programName.lastIndexOf(":")) else programName
    val v: String = if(isGAV) programName.substring(programName.lastIndexOf(":") + 1) else programName

    val publishedAt: String = uploadTime

    def getPackages: Set[JavaPackage] = getChildren.map(_.asInstanceOf[JavaPackage])

    def allClasses: Set[JavaClass] = getPackages.flatMap(_.getClasses)

    def allMethods: Set[JavaMethod] = allClasses.flatMap(_.getMethods)

  }

  class JavaPackage(packageName: String,
                    entityId: Long,
                    repositoryIdent: String) extends PathIdentifiableJavaEntity(packageName, packageName, entityId, repositoryIdent, None) {
    override val kind: SoftwareEntityKind = SoftwareEntityKind.Package

    def getClasses: Set[JavaClass] = getChildren.map(_.asInstanceOf[JavaClass])

    def allMethods: Set[JavaMethod] = getClasses.flatMap(_.getMethods)
  }

  class JavaClass(className: String,
                  thisTypeFqn: String,
                  entityId: Long,
                  superTypeFqn: Option[String],
                  interfaceFqns: Set[String],
                  interfaceType: Boolean,
                  finalType: Boolean,
                  abstractType: Boolean,
                  repositoryIdent: String,
                  hashedBytes: Array[Byte]) extends PathIdentifiableJavaEntity(className, className, entityId, repositoryIdent, Some(hashedBytes)){
    override val kind: SoftwareEntityKind = SoftwareEntityKind.Class

    lazy val methodTable: Map[String, Map[String, JavaMethod]] = getMethods.groupBy(_.name).view.mapValues{ jmSet =>
      jmSet.groupBy(_.descriptor).view.mapValues(_.head).toMap
    }.toMap

    val thisType: String = thisTypeFqn
    val superType: Option[String] = superTypeFqn
    val interfaceTypes: Set[String]= interfaceFqns
    val isInterface: Boolean = interfaceType
    val isFinal: Boolean = finalType
    val isAbstract: Boolean = abstractType

    def getMethods: Set[JavaMethod] = getChildren.map(_.asInstanceOf[JavaMethod])
    def lookupMethod(name: String, descriptor: String): Option[JavaMethod] = methodTable.get(name).flatMap(m => m.get(descriptor))
  }

  def buildMethodIdent(methodName: String, jvmDescriptor: String) = s"$methodName: $jvmDescriptor"

  class JavaMethod(methodName: String,
                   jvmDescriptor: String,
                   entityId: Long,
                   finalMethod: Boolean,
                   staticMethod: Boolean,
                   abstractMethod: Boolean,
                   methodVisibility: String,
                   repositoryIdent: String,
                   hash: Int) extends PathIdentifiableJavaEntity(methodName, buildMethodIdent(methodName, jvmDescriptor), entityId, repositoryIdent, None){

    override val kind: SoftwareEntityKind = SoftwareEntityKind.Method

    private lazy val opalDescriptor = MethodDescriptor(jvmDescriptor)

    lazy val statements: Seq[JavaStatement] = getChildren.map(_.asInstanceOf[JavaStatement]).toSeq.sortBy(_.instructionPc)
    lazy val newStatements: Seq[JavaNewInstanceStatement] = getChildren.collect { case x: JavaNewInstanceStatement => x }.toSeq.sortBy(_.instructionPc)
    lazy val invocationStatements: Seq[JavaInvokeStatement] = getChildren.collect { case x: JavaInvokeStatement => x }.toSeq.sortBy(_.instructionPc)
    lazy val enclosingClass: Option[JavaClass] = getParent.map(_.asInstanceOf[JavaClass])
    lazy val fieldAccessStatements: Seq[JavaFieldAccessStatement] = getChildren.collect{ case x: JavaFieldAccessStatement => x }.toSeq.sortBy(_.instructionPc)

    val descriptor: String = jvmDescriptor
    val isFinal: Boolean = finalMethod
    val isStatic: Boolean = staticMethod
    val isAbstract: Boolean = abstractMethod
    val visibility: String = methodVisibility
    val methodHash: Int = hash

    def paramTypes: Seq[String] = opalDescriptor.parameterTypes.map(_.toJVMTypeName)
    def returnType: String = opalDescriptor.returnType.toJVMTypeName

    override def toString: String = enclosingClass.map(_.thisType).getOrElse("<no-class>") + "->" + methodName + jvmDescriptor
  }

  abstract class JavaStatement(name: String, pc: Int, entityId: Long, repositoryIdent: String)
    extends PathIdentifiableJavaEntity(name, String.valueOf(pc), entityId, repositoryIdent, None){
    val instructionPc: Int = pc
  }

  class JavaInvokeStatement(methodName: String,
                            declaredTypeFqn: String,
                            jvmDescriptor: String,
                            invocationType: JavaInvocationType,
                            pc: Int,
                            entityId: Long,
                            repositoryIdent: String) extends JavaStatement(methodName, pc, entityId, repositoryIdent) {
    override val kind: SoftwareEntityKind = SoftwareEntityKind.InvocationStatement

    val targetMethodName: String = methodName
    val targetDescriptor: String = jvmDescriptor
    val targetTypeName: String = declaredTypeFqn
    val isStaticMethod: Boolean = invocationType == JavaInvocationType.Static
    val invokeStatementType: JavaInvocationType = invocationType

    override def toString: String = s"[${invokeStatementType.toString}] $declaredTypeFqn.$methodName : $jvmDescriptor"
  }

  class JavaFieldAccessStatement(fieldName: String,
                                 fieldTypeFqn: String,
                                 declaredTypeFqn: String,
                                 accessType: JavaFieldAccessType,
                                 pc:Int,
                                 entityId: Long,
                                 repositoryIdent: String) extends JavaStatement(fieldName, pc, entityId, repositoryIdent) {
    override val kind: SoftwareEntityKind = SoftwareEntityKind.FieldAccessStatement

    val targetFieldName: String = fieldName
    val targetFieldTypeName: String = fieldTypeFqn
    val targetTypeName: String = declaredTypeFqn
    val fieldAccessType: JavaFieldAccessType = accessType
  }

  class JavaNewInstanceStatement(typeName: String, pc: Int, entityId: Long, repositoryIdent: String)
    extends JavaStatement(typeName, pc, entityId, repositoryIdent){
    override val kind: SoftwareEntityKind = SoftwareEntityKind.NewInstanceStatement
    val instantiatedTypeName: String = typeName
  }

  object JavaInvocationType extends Enumeration {
    type JavaInvocationType = Value

    val Virtual: Value = Value(1)
    val Special: Value = Value(2)
    val Interface: Value = Value(3)
    val Static: Value = Value(4)
    val Dynamic: Value = Value(5)


    def fromId(id: Int): JavaInvocationType = id match {
      case 1 => Virtual
      case 2 => Special
      case 3 => Interface
      case 4 => Static
      case 5 => Dynamic
      case _ => throw new IllegalArgumentException(s"Invalid id for InvocationType $id")
    }
  }

  object JavaFieldAccessType extends Enumeration {
    type JavaFieldAccessType = Value

    val StaticPut: Value = Value(1)
    val StaticGet: Value = Value(2)
    val InstancePut: Value = Value(3)
    val InstanceGet: Value = Value(4)

    def fromId(id: Int): JavaFieldAccessType = id match {
      case 1 => StaticPut
      case 2 => StaticGet
      case 3 => InstancePut
      case 4 => InstanceGet
      case _ => throw new IllegalArgumentException(s"Invalid id for FieldAccessType $id")
    }
  }


}
