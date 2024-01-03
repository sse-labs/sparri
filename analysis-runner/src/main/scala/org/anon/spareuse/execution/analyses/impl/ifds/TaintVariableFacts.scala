package org.anon.spareuse.execution.analyses.impl.ifds

import org.opalj.br.{FieldType, Method, ObjectType}

import scala.collection.mutable

object TaintVariableFacts {

  private def normalizeVarName(localVar: TACVar): String = localVar match {
    case uvar: TACUVar =>
      uvar.name.substring(1, uvar.name.length - 1)
    case dvar: TACDVar =>
      dvar.name
    case u@_ =>
      throw new RuntimeException(s"Unexpected variable type: ${u.getClass}")
  }

  trait TaintVariable extends IFDSFact

  case class LocalTaintVariable(variable: TACVar) extends TaintVariable {
    override def uniqueIdent: String = s"<LOCAL> ${normalizeVarName(variable)}: ${variable.cTpe.getClass}"

    override def toString: String = uniqueIdent

    override def displayName: String = normalizeVarName(variable)
  }

  case class TaintField(declaringClass: ObjectType, fieldType: FieldType, name: String, isStatic: Boolean) extends TaintVariable {

    override def uniqueIdent: String = {
      val builder = new mutable.StringBuilder("<FIELD> ")
      builder.append(declaringClass.fqn)
      if (isStatic) builder.append(" static")
      builder.append(" ")
      builder.append(name)
      builder.append(" : ")
      builder.append(fieldType.toJava)
      builder.toString()
    }

    override def toString: String = uniqueIdent

    override def displayName: String = declaringClass.fqn + "." + name
  }

  case class TaintFunctionReturn(context: Method, callStmt: CallStatementNode) extends TaintVariable {
    override def uniqueIdent: String = context.toJava + callStmt.stmt.toString

    override def toString: String = uniqueIdent

    override def displayName: String = s"<CALL-RETURN [pc=${callStmt.stmt.pc}] >"
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
  def buildFact(context: Method, call: CallStatementNode): IFDSFact = TaintFunctionReturn(context ,call)

  def buildFact(declClass: ObjectType, fieldType: FieldType, name: String, isStatic: Boolean): IFDSFact = {
    if (fieldMap.contains(declClass) && fieldMap(declClass).contains(name)) fieldMap(declClass)(name)
    else {
      val fact = TaintField(declClass, fieldType, name, isStatic)

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
}
