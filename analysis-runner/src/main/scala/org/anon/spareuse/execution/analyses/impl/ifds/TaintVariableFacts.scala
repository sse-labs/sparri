package org.anon.spareuse.execution.analyses.impl.ifds

import org.opalj.br.{FieldType, ObjectType}

import scala.annotation.switch
import scala.collection.mutable

object TaintVariableFacts {

  private[ifds] def normalizeVarName(localVar: TACVar): String = localVar match {
    case uvar: TACUVar =>
      uvar.name.substring(1, uvar.name.length - 1)
    case dvar: TACDVar =>
      dvar.name
    case u@_ =>
      throw new RuntimeException(s"Unexpected variable type: ${u.getClass}")
  }

  trait TaintVariable extends IFDSFact

  case class LocalTaintVariable(normalVariableName: String) extends TaintVariable {
    override def uniqueIdent: String = s"<LOCAL> $normalVariableName"

    override def toString: String = uniqueIdent

    override def displayName: String = normalVariableName
  }

  object LocalTaintVariable {
    def apply(variable: TACVar): LocalTaintVariable = LocalTaintVariable(normalizeVarName(variable))
    def apply(variable: LocalVariable): LocalTaintVariable = LocalTaintVariable(variable.variableName)
  }

  case class TaintField(declaringClassFqn: String, fieldTypeName: String, name: String, isStatic: Boolean) extends TaintVariable {

    override def uniqueIdent: String = {
      val builder = new mutable.StringBuilder("<FIELD> ")
      builder.append(declaringClassFqn)
      if (isStatic) builder.append(" static")
      builder.append(" ")
      builder.append(name)
      builder.append(" : ")
      builder.append(fieldTypeName)
      builder.toString()
    }

    override def toString: String = uniqueIdent

    override def displayName: String = declaringClassFqn + "." + name
  }

  case class TaintFunctionReturn(callPc: Int) extends TaintVariable {
    override def uniqueIdent: String = s"<CALL-RETURN> $callPc"

    override def toString: String = uniqueIdent

    override def displayName: String = s"<CALL-RETURN> [pc=$callPc]"
  }


  private val localVarMap: mutable.Map[String, IFDSFact] = new mutable.HashMap[String, IFDSFact]()
  private val fieldMap: mutable.Map[ObjectType, mutable.Map[String, IFDSFact]] = new mutable.HashMap[ObjectType, mutable.Map[String, IFDSFact]]

  def clearLocals(): Unit = localVarMap.clear()

  def buildFact(localVar: TACVar): IFDSFact = {
    val varName = normalizeVarName(localVar)
    if (localVarMap.contains(varName)) localVarMap(varName)
    else {
      val fact = LocalTaintVariable(localVar)
      localVarMap.put(varName, fact)
      fact
    }
  }

  def buildFact(localVar: LocalVariable): IFDSFact = {
    if (localVarMap.contains(localVar.variableName)) localVarMap(localVar.variableName)
    else {
      val fact = LocalTaintVariable(localVar)
      localVarMap.put(localVar.variableName, fact)
      fact
    }
  }

  def buildFact(call: CallStatementNode): IFDSFact = TaintFunctionReturn(call.stmtPc)

  def buildFact(declClass: ObjectType, fieldType: FieldType, name: String, isStatic: Boolean): IFDSFact = {
    if (fieldMap.contains(declClass) && fieldMap(declClass).contains(name)) fieldMap(declClass)(name)
    else {
      val fact = TaintField(declClass.fqn, fieldType.toJVMTypeName, name, isStatic)

      if (fieldMap.contains(declClass)) {
        fieldMap(declClass).put(name, fact)
      } else {
        val map = new mutable.HashMap[String, IFDSFact]()
        map.put(name, fact)
        fieldMap.put(declClass, map)
      }

      fact
    }
  }

  def parseFact(uniqueIdent: String): IFDSFact = {
    val splits = uniqueIdent.split(" ")

    if(splits.length < 2) throw new IllegalArgumentException(s"Not a valid fact identifier: $uniqueIdent")

    (uniqueIdent.split(" ").head.trim: @switch) match {
      case "<LOCAL>" => LocalTaintVariable(splits(1))
      case "<FIELD>" if splits.length == 5 || splits.length == 6 =>
        val classFqn = splits(1)
        val isStatic = splits.length == 6
        val name = if(isStatic) splits(3) else splits(2)
        val fieldTypeName = if(isStatic) splits(5) else splits(4)
        TaintField(classFqn, fieldTypeName, name, isStatic)
      case "<CALL-RETURN>" => TaintFunctionReturn(splits(1).toInt)
    }

  }
}
