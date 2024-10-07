package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.AnalysisRunData
import org.anon.spareuse.execution.analyses.AnalysisImplementationDescriptor
import org.anon.spareuse.execution.analyses.impl.ifds.TaintVariableFacts.TaintVariable
import org.opalj.br.{Method, MethodDescriptor, ObjectType}
import org.opalj.tac.{Assignment, BinaryExpr, Const, Expr, ExprStmt, FunctionCall, GetField, GetStatic, InstanceFunctionCall, InstanceMethodCall, PutField, PutStatic}

class IFDSTaintFlowSummaryBuilderImpl(baselineRunOpt: Option[AnalysisRunData]) extends DefaultIFDSSummaryBuilder(baselineRunOpt) {

  override val descriptor: AnalysisImplementationDescriptor =  IFDSTaintFlowSummaryBuilderImpl.descriptor

  override protected[ifds] def analyzeStatement(currentNode: StatementNode, currentStatement: TACStmt, currentMethod: Method, graph: IFDSMethodGraph)(implicit tac: MethodTAC): Unit = currentStatement match {
    case Assignment(_, targetVar: TACDVar, assignedExpr) =>
      val targetFact = TaintVariableFacts.buildFact(targetVar).asTaintVariable
      analyzeExpression(currentNode, targetFact, assignedExpr, currentMethod)

    case PutField(_, declClass, name, fieldType, _, valueExpr) if fieldType == ObjectType.String =>
      val targetFact = TaintVariableFacts.buildFact(declClass, fieldType, name, isStatic = false).asTaintVariable
      analyzeExpression(currentNode, targetFact, valueExpr, currentMethod)

    case PutStatic(_, declClass, name, fieldType, valueExpr) if fieldType == ObjectType.String =>
      val targetFact = TaintVariableFacts.buildFact(declClass, fieldType, name, isStatic = true).asTaintVariable
      analyzeExpression(currentNode, targetFact, valueExpr, currentMethod)

    case expr: ExprStmt[TACVar] if expr.expr.isInstanceOf[InstanceFunctionCall[TACVar]] =>
      val sbCall = expr.expr.asInstanceFunctionCall
      val receiverFacts = getAllReceiverFacts(sbCall.receiver.asVar)
      receiverFacts.foreach{ rec => handleStringBuilderInvocation(currentNode, None, rec, sbCall.name, sbCall.descriptor, sbCall.params) }

    case call: InstanceMethodCall[TACVar] if call.declaringClass.isObjectType && call.declaringClass.asObjectType.fqn == ObjectType.StringBuilder.fqn =>
      val receiverFacts = getAllReceiverFacts(call.receiver.asVar)
      receiverFacts.foreach{ rec => handleStringBuilderInvocation(currentNode, None, rec, call.name, call.descriptor, call.params) }

    case _ =>
      // Strings are immutable, so passing them to method invocations cannot ever change (ie. taint / untaint) them
      // Ergo, we only have to consider assignments / field stores
  }

  private[ifds] def getAllReceiverFacts(receiver: TACVar)(implicit tac: MethodTAC): Set[IFDSFact] = {
    if (receiver.definedBy.size > 1) {
      receiver.definedBy.map(tac.stmts).map(_.asAssignment.targetVar).map(TaintVariableFacts.buildFact)
    } else {
      Set(TaintVariableFacts.buildFact(receiver))
    }
  }

  private[ifds] def handleStringBuilderInvocation(currentNode: StatementNode,
                                                  targetVar: Option[IFDSFact],
                                                  callReceiverVar: IFDSFact,
                                                  name: String,
                                                  descriptor: MethodDescriptor,
                                                  params: Seq[Expr[TACVar]]): Unit = name match {
    case "<init>" if descriptor.parametersCount == 1 && descriptor.parameterTypes(0) == ObjectType.String =>
      val paramFact = TaintVariableFacts.buildFact(params.head.asVar)
      currentNode.setGeneratesOn(callReceiverVar, Set(paramFact))
    case "append" if descriptor.parametersCount == 1 && descriptor.parameterTypes(0) == ObjectType.String || descriptor.parameterTypes(0) == ObjectType.StringBuilder=>
      val paramFact = TaintVariableFacts.buildFact(params.head.asVar)
      currentNode.setGeneratesOn(callReceiverVar, Set(callReceiverVar, paramFact))
      if(targetVar.isDefined) currentNode.setGeneratesOn(targetVar.get, Set(callReceiverVar, paramFact))
    case "insert" if descriptor.parametersCount == 2 && descriptor.parameterTypes(1) == ObjectType.String =>
      val stringParamFact = TaintVariableFacts.buildFact(params(1).asVar)
      currentNode.setGeneratesOn(callReceiverVar, Set(callReceiverVar, stringParamFact))
      if (targetVar.isDefined) currentNode.setGeneratesOn(targetVar.get, Set(callReceiverVar, stringParamFact))
    case "replace" if descriptor.parametersCount == 3 && descriptor.parameterTypes(2) == ObjectType.String =>
      val stringParamFact = TaintVariableFacts.buildFact(params(2).asVar)
      currentNode.setGeneratesOn(callReceiverVar, Set(callReceiverVar, stringParamFact))
      if(targetVar.isDefined) currentNode.setGeneratesOn(targetVar.get, Set(callReceiverVar, stringParamFact))
    case "toString" if targetVar.isDefined =>
      currentNode.setGeneratesOn(targetVar.get, Set(callReceiverVar))
    case n@_ =>
      log.info(s"Untracked StringBuilder invocation: $n")
  }

  private[ifds] def analyzeExpression(currentNode: StatementNode, targetFact: TaintVariable, expression: Expr[TACVar], currentMethod: Method)(implicit tac: MethodTAC): Unit = expression match {
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
      val enablingFacts = getAllReceiverFacts(local)
      currentNode.setGeneratesOn(targetFact, enablingFacts)
    case BinaryExpr(_, _, _, left, right) =>
      log.info(s"Found a binary expression assigned to a string: $expression")
      analyzeExpression(currentNode, targetFact, left, currentMethod)
      analyzeExpression(currentNode, targetFact, right, currentMethod)
    case fc: FunctionCall[TACVar] if fc.name.equalsIgnoreCase("source") =>
      // This is the hardcoded source for taint: All methods named "source" (for now)
      currentNode.setGeneratesFact(targetFact)

    case ifc: InstanceFunctionCall[TACVar] if ifc.receiver.isVar && ifc.name == "concat" =>
      // Concatenations do not need to be resolved, they are hardcoded to carry taint if the receiver or parameter carries taint
      val paramFacts = ifc.params.collect {
        case localVar: TACVar =>
          getAllReceiverFacts(localVar)
      }.flatten

      val sourceFacts = getAllReceiverFacts(ifc.receiver.asVar)

      currentNode.setGeneratesOn(targetFact, sourceFacts ++ paramFacts)


    case sbCall: InstanceFunctionCall[TACVar] if sbCall.declaringClass.isObjectType && sbCall.declaringClass.asObjectType == ObjectType.StringBuilder =>
      val receiverVars = getAllReceiverFacts(sbCall.receiver.asVar)

      receiverVars.foreach{ rec => handleStringBuilderInvocation(currentNode, Some(targetFact), rec, sbCall.name, sbCall.descriptor, sbCall.params) }

    case sb: InstanceFunctionCall[TACVar] if sb.name == "toString" =>
      if (sb.declaringClass == ObjectType.String) {
        val receiverFacts = getAllReceiverFacts(sb.receiver.asVar)
        currentNode.setGeneratesOn(targetFact, receiverFacts)
      } else {
        //toString on anything else than String will kill taint.
        currentNode.setKillsFact(targetFact)
      }

    case _ : FunctionCall[TACVar] =>
      // For the general function call: Create artificial return node to represent taint of the target variable
      // TODO: Special calls to sanatizers should be handled differently
      val sourceFact = TaintVariableFacts.buildFact(currentNode.asCallNode)
      currentNode.setGeneratesOn(targetFact, Set(sourceFact))


    case other@_ =>
      log.warn(s"Unhandled expression assigned to ${targetFact.displayName}: $other")


  }
}

object IFDSTaintFlowSummaryBuilderImpl {

  val analysisName: String = "TaintFlowSummaryBuilder"
  val analysisVersion: String = "0.0.1"
  val analysisDescription: String = "This analysis builds IFDS taint flow summaries for hardcoded sources and sinks tracking only strings"

  val descriptor: AnalysisImplementationDescriptor = DefaultIFDSSummaryBuilder.buildDescriptor(analysisName, analysisVersion, analysisDescription)

}
