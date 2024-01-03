package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.AnalysisRunData
import org.opalj.br.{Method, ObjectType}
import org.opalj.tac.{Assignment, BinaryExpr, Const, Expr, FunctionCall, GetField, GetStatic, PutField, PutStatic}
import org.opalj.value.IsStringValue

class IFDSTaintFlowSummaryBuilderImpl(baselineRunOpt: Option[AnalysisRunData]) extends DefaultIFDSSummaryBuilder(baselineRunOpt) {
  override protected val analysisName: String = "TaintFlowSummaryBuilder"
  override protected val analysisVersion: String = "0.0.1"
  override protected val analysisDescription: String = "This analysis builds IFDS taint flow summaries for hardcoded sources and sinks tracking only strings"


  override protected[ifds] def analyzeMethod(method: Method)(implicit TACAIProvider: MethodTACProvider): IFDSMethodGraph = {
    // Make sure local variable fact cache is cleared after method has been processed
    val result = super.analyzeMethod(method)
    TaintVariableFacts.clearLocals()
    result
  }

  override protected[ifds] def analyzeStatement(currentNode: StatementNode, currentMethod: Method, graph: IFDSMethodGraph): Unit = currentNode.stmt match {
    case Assignment(_, targetVar: TACDVar, assignedExpr) if targetVar.value.isInstanceOf[IsStringValue]=>
      val targetFact = TaintVariableFacts.buildFact(targetVar)
      analyzeExpression(currentNode, targetFact, assignedExpr, currentMethod)

    case PutField(_, declClass, name, fieldType, _, valueExpr) if fieldType == ObjectType.String =>
      val targetFact = TaintVariableFacts.buildFact(declClass, fieldType, name, isStatic = false)
      analyzeExpression(currentNode, targetFact, valueExpr, currentMethod)

    case PutStatic(_, declClass, name, fieldType, valueExpr) if fieldType == ObjectType.String =>
      val targetFact = TaintVariableFacts.buildFact(declClass, fieldType, name, isStatic = true)
      analyzeExpression(currentNode, targetFact, valueExpr, currentMethod)

    case _ =>
      // Strings are immutable, so passing them to method invocations cannot ever change (ie. taint / untaint) them
      // Ergo, we only have to consider assignments / field stores
  }

  private[ifds] def analyzeExpression(currentNode: StatementNode, targetFact: IFDSFact, expression: Expr[TACVar], currentMethod: Method): Unit = expression match {
    case _ : Const =>
      // If a constant is assigned to our current variable, all taint is cleared
      currentNode.setKillsFact(targetFact)
    case gf: GetField[TACVar] =>
      // If a field is loaded, our current variables taint is equal to the field's taint
      val enablingFact = TaintVariableFacts.buildFact(gf.declaringClass, gf.declaredFieldType, gf.name, isStatic = false)
      currentNode.setGeneratesOn(targetFact, Set(enablingFact))
    case gs: GetStatic =>
      // If a static field is loaded, our current variables taint is equal to the field's taint
      val enablingFact = TaintVariableFacts.buildFact(gs.declaringClass, gs.declaredFieldType, gs.name, isStatic = true)
      currentNode.setGeneratesOn(targetFact, Set(enablingFact))
    case local: TACVar =>
      // If another local variable is assigned, our current variable's taint is equal to the other variable's taint
      val enablingFact = TaintVariableFacts.buildFact(local)
      currentNode.setGeneratesOn(targetFact, Set(enablingFact))
    case BinaryExpr(_, _, _, left, right) =>
      //TODO: Find out if this matches for TAC in case of String res = "a" + "b";
      analyzeExpression(currentNode, targetFact, left, currentMethod)
      analyzeExpression(currentNode, targetFact, right, currentMethod)
    case fc: FunctionCall[TACVar] if fc.name.equalsIgnoreCase("source") =>
      // This is the hardcoded source for taint: All methods named "source" (for now)
      currentNode.setGeneratesFact(targetFact)

    case fc: FunctionCall[TACVar] =>
      //TODO: Whether or not there must be taint assigned depends on the receiver object (for instance calls). We need to store the receiver variable somewhere
      //TODO: Have special handling for sanatizers
      val sourceFact = TaintVariableFacts.buildFact(currentMethod, currentNode.asCallNode)
      currentNode.setGeneratesOn(targetFact, Set(sourceFact))


  }
}
