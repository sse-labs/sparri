package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.AnalysisRunData
import org.anon.spareuse.execution.analyses.AnalysisImplementationDescriptor
import org.opalj.br.{Method, MethodDescriptor, ObjectType}
import org.opalj.tac.{Assignment, BinaryExpr, Const, Expr, ExprStmt, FunctionCall, GetField, GetStatic, InstanceFunctionCall, InstanceMethodCall, PutField, PutStatic}

class IFDSTaintFlowSummaryBuilderImpl(baselineRunOpt: Option[AnalysisRunData]) extends DefaultIFDSSummaryBuilder(baselineRunOpt) {

  override val descriptor: AnalysisImplementationDescriptor =  IFDSTaintFlowSummaryBuilderImpl.descriptor

  override protected[ifds] def analyzeStatement(currentNode: StatementNode, currentStatement: TACStmt, currentMethod: Method, graph: IFDSMethodGraph): Unit = currentStatement match {
    case Assignment(_, targetVar: TACDVar, assignedExpr) =>
      val targetFact = TaintVariableFacts.buildFact(targetVar)
      analyzeExpression(currentNode, targetFact, assignedExpr, currentMethod)

    case PutField(_, declClass, name, fieldType, _, valueExpr) if fieldType == ObjectType.String =>
      val targetFact = TaintVariableFacts.buildFact(declClass, fieldType, name, isStatic = false)
      analyzeExpression(currentNode, targetFact, valueExpr, currentMethod)

    case PutStatic(_, declClass, name, fieldType, valueExpr) if fieldType == ObjectType.String =>
      val targetFact = TaintVariableFacts.buildFact(declClass, fieldType, name, isStatic = true)
      analyzeExpression(currentNode, targetFact, valueExpr, currentMethod)

    case expr: ExprStmt[TACVar] if expr.expr.isInstanceOf[InstanceFunctionCall[TACVar]] =>
      val sbCall = expr.expr.asInstanceFunctionCall
      val receiverVar = TaintVariableFacts.buildFact(sbCall.receiver.asVar)
      handleStringBuilderInvocation(currentNode, None, receiverVar, sbCall.name, sbCall.descriptor, sbCall.params)

    case call: InstanceMethodCall[TACVar] if call.declaringClass.isObjectType && call.declaringClass.asObjectType.fqn == ObjectType.StringBuilder.fqn =>
      val receiverFact = TaintVariableFacts.buildFact(call.receiver.asVar)

      handleStringBuilderInvocation(currentNode, None, receiverFact, call.name, call.descriptor, call.params)

    case _ =>
      // Strings are immutable, so passing them to method invocations cannot ever change (ie. taint / untaint) them
      // Ergo, we only have to consider assignments / field stores
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
      val definingParameterIdx = local.definedBy.filter( _ < -1).toList.map( defIdx => -1 * defIdx - 2)

      if(definingParameterIdx.nonEmpty){
        log.info(s"Variable ${local.name} is defined by parameter with index: ${definingParameterIdx.mkString(",")}")
        //TODO: Mark variables that originate from parameters here and in function calls!
      }

      val enablingFact = TaintVariableFacts.buildFact(local)
      currentNode.setGeneratesOn(targetFact, Set(enablingFact))
    case BinaryExpr(_, _, _, left, right) =>
      log.info(s"Found a binary expression assigned to a string: $expression")
      analyzeExpression(currentNode, targetFact, left, currentMethod)
      analyzeExpression(currentNode, targetFact, right, currentMethod)
    case fc: FunctionCall[TACVar] if fc.name.equalsIgnoreCase("source") =>
      // This is the hardcoded source for taint: All methods named "source" (for now)
      currentNode.setGeneratesFact(targetFact)

    case ifc: InstanceFunctionCall[TACVar] if ifc.receiver.isVar && ifc.name == "concat" =>
      // Concatinations do not need to be resolved, they are hardcoded to carry taint if the receiver or parameter carries taint
      val paramFacts = ifc.params.collect {
        case localVar: TACVar =>
          TaintVariableFacts.buildFact(localVar)
      }

      val sourceFact = TaintVariableFacts.buildFact(ifc.receiver.asVar)

      currentNode.setGeneratesOn(targetFact, paramFacts.toSet ++ Set(sourceFact))


    case sbCall: InstanceFunctionCall[TACVar] if sbCall.declaringClass.isObjectType && sbCall.declaringClass.asObjectType == ObjectType.StringBuilder =>
      val receiverVar = TaintVariableFacts.buildFact(sbCall.receiver.asVar)
      handleStringBuilderInvocation(currentNode, Some(targetFact), receiverVar, sbCall.name, sbCall.descriptor, sbCall.params)

    case sb: InstanceFunctionCall[TACVar] if sb.name == "toString" =>
      if (sb.declaringClass == ObjectType.String) {
        val receiverFact = TaintVariableFacts.buildFact(sb.receiver.asVar)
        currentNode.setGeneratesOn(targetFact, Set(receiverFact))
      } else {
        //toString on anything else than Stringwill kill taint.
        currentNode.setKillsFact(targetFact)
      }

    case fc: FunctionCall[TACVar] =>
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
