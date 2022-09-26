package de.tudo.sse.spareuse.core.model.entities

import de.tudo.sse.spareuse.core.model.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaFieldAccessType.JavaFieldAccessType
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaInvocationType.JavaInvocationType

object JavaEntities {

  abstract class PathIdentifiableJavaEntity private[entities] (entityName: String,
                                                     entityIdent: String,
                                                     repositoryIdent: String) extends SoftwareEntityData {
    override val name: String = entityName
    override val language: String = "java"
    override val repository: String = repositoryIdent
    override def uid: String = getParent.map(p => p.uid + "!" + entityIdent).getOrElse(entityIdent)
  }

  class JavaProgram(programName: String,
                    programIdent: String,
                    repositoryIdent: String) extends PathIdentifiableJavaEntity(programName, programIdent, repositoryIdent) {

    override val kind: SoftwareEntityKind = SoftwareEntityKind.Program

  }

  class JavaPackage(packageName: String,
                    repositoryIdent: String) extends PathIdentifiableJavaEntity(packageName, packageName, repositoryIdent) {
    override val kind: SoftwareEntityKind = SoftwareEntityKind.Package
  }

  class JavaClass(className: String,
                  thisTypeFqn: String,
                  superTypeFqn: Option[String],
                  repositoryIdent: String) extends PathIdentifiableJavaEntity(className, thisTypeFqn, repositoryIdent){
    override val kind: SoftwareEntityKind = SoftwareEntityKind.Class

    val thisType: String = thisTypeFqn
    val superType: Option[String] = superTypeFqn
  }

  private def buildMethodIdent(methodName: String, returnType: String, paramTypes: Seq[String]) =
    s"$returnType $methodName(${paramTypes.mkString(",")})"

  class JavaMethod(methodName: String,
                   returnTypeFqn: String,
                   paramTypeNames: Seq[String],
                   repositoryIdent: String) extends PathIdentifiableJavaEntity(methodName, buildMethodIdent(methodName, returnTypeFqn, paramTypeNames), repositoryIdent){

    override val kind: SoftwareEntityKind = SoftwareEntityKind.Method

    val returnType: String = returnTypeFqn
    val paramCount: Int = paramTypeNames.size
    val paramTypes: Seq[String] = paramTypeNames
  }

  abstract class JavaStatement(name: String, pc: Int, repositoryIdent: String) extends PathIdentifiableJavaEntity(name, String.valueOf(pc), repositoryIdent){
    val instructionPc: Int = pc
  }

  class JavaInvokeStatement(methodName: String,
                            declaredTypeFqn: String,
                            paramCount: Int,
                            returnTypeFqn: String,
                            invocationType: JavaInvocationType,
                            pc: Int,
                            repositoryIdent: String) extends JavaStatement(methodName, pc, repositoryIdent) {
    override val kind: SoftwareEntityKind = SoftwareEntityKind.InvocationStatement

    val targetMethodName: String = methodName
    val targetMethodParameterCount: Int = paramCount
    val returnTypeName: String = returnTypeFqn
    val targetTypeName: String = declaredTypeFqn
    val isStaticMethod: Boolean = invocationType == JavaInvocationType.Static
    val invokeStatementType: JavaInvocationType = invocationType
  }

  class JavaFieldAccessStatement(fieldName: String,
                                 fieldTypeFqn: String,
                                 declaredTypeFqn: String,
                                 accessType: JavaFieldAccessType,
                                 pc:Int,
                                 repositoryIdent: String) extends JavaStatement(fieldName, pc, repositoryIdent) {
    override val kind: SoftwareEntityKind = SoftwareEntityKind.FieldAccessStatement

    val targetFieldName: String = fieldName
    val targetFieldTypeName: String = fieldTypeFqn
    val targetTypeName: String = declaredTypeFqn
    val fieldAccessType: JavaFieldAccessType = accessType
  }

  object JavaInvocationType extends Enumeration {
    type JavaInvocationType = Value

    val Virtual: Value = Value(1)
    val Special: Value = Value(2)
    val Interface: Value = Value(3)
    val Static: Value = Value(4)
    val Dynamic: Value = Value(5)
  }

  object JavaFieldAccessType extends Enumeration {
    type JavaFieldAccessType = Value

    val StaticPut: Value = Value(1)
    val StaticGet: Value = Value(2)
    val InstancePut: Value = Value(3)
    val InstanceGet: Value = Value(4)
  }


}
