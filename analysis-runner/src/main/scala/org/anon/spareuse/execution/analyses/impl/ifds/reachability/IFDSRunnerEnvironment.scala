package org.anon.spareuse.execution.analyses.impl.ifds.reachability

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.{CallStatementNode, IFDSFact, IFDSMethodGraph, StatementNode}
import org.anon.spareuse.execution.analyses.impl.ifds.reachability.IFDSMethodRunner.CallGraph
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class IFDSRunnerEnvironment private(cg: CallGraph, summaryLookup: Map[MethodIdent, IFDSMethodGraph]) {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private val methodStatementsToFactMap: mutable.Map[MethodIdent, mutable.Map[Int, Set[IFDSFact]]] = mutable.Map.empty
  private val methodReturnFacts: mutable.Map[MethodIdent, mutable.Set[IFDSFact]] = mutable.Map.empty
  private val methodReturnTainted: mutable.Map[MethodIdent, Boolean] = mutable.Map.empty
  private val methodReturnNodes: mutable.Map[MethodIdent, mutable.Set[(MethodIdent, StatementNode)]] = mutable.Map.empty

  def hasSummary(ident: MethodIdent): Boolean = summaryLookup.contains(ident)
  def getSummary(ident: MethodIdent): IFDSMethodGraph = summaryLookup(ident)

  def setVisitedWith(method: MethodIdent, stmtIdx: Int, activations: Set[IFDSFact]): Unit = {
    if(!wasVisited(method))
      methodStatementsToFactMap(method) = mutable.Map(stmtIdx -> activations)
    else if(!methodStatementsToFactMap(method).contains(stmtIdx))
      methodStatementsToFactMap(method)(stmtIdx) = activations
    else
      methodStatementsToFactMap(method)(stmtIdx) = methodStatementsToFactMap(method)(stmtIdx).union(activations)
  }

  def wasVisited(method: MethodIdent): Boolean = methodStatementsToFactMap.contains(method)

  def newActivationsAt(method: MethodIdent, stmtIdx: Int, activations: Set[IFDSFact]): Set[IFDSFact] = {
    val prevFacts = methodStatementsToFactMap.get(method).flatMap(_.get(stmtIdx)).getOrElse(Set.empty)
    activations.diff(prevFacts)
  }

  def setMethodReturnsFacts(method: MethodIdent, returnedFacts: Set[IFDSFact]): Unit = {
    if(!methodReturnFacts.contains(method))
      methodReturnFacts(method) = mutable.Set.from(returnedFacts)
    else
      methodReturnFacts(method).addAll(returnedFacts)

    summaryLookup.get(method) match {
      case Some(ifdsGraph) =>
        if(returnedFacts.exists(returnedFact => ifdsGraph.allVariablesReturned.contains(returnedFact)))
          methodReturnTainted(method) = true
      case None =>
    }
  }

  def setMethodReturnsTo(callee: MethodIdent, caller: MethodIdent, callSuccessor: StatementNode): Unit = {
    if(!methodReturnNodes.contains(callee)) methodReturnNodes(callee) = mutable.Set.empty

    methodReturnNodes(callee).add((caller, callSuccessor))
  }

  def getMethodReturnsTo(method: MethodIdent): Option[Set[(MethodIdent, StatementNode)]] = methodReturnNodes.get(method).map(_.toSet)

  def methodReturnIsTainted(method: MethodIdent): Boolean = {
    methodReturnTainted.getOrElse(method, false)
  }

  def methodResultingFacts(method: MethodIdent): Set[IFDSFact] = {
    methodReturnFacts.get(method).map(_.toSet).getOrElse(Set.empty)
  }

  def getTargetMethodSummaries(csn: CallStatementNode, caller: MethodIdent): Set[IFDSMethodGraph] = {
    cg
      .calleesOf(caller)
      .collect {
        case (csn.stmtPc, callee) =>
          callee
      }
      .flatten
      .flatMap { calleeMethod =>
        assert(calleeMethod.methodName == csn.functionName && calleeMethod.descriptor == csn.descriptor)
        if(!hasSummary(calleeMethod.methodIdentifier)){
          log.warn(s"No summary for target method of call: ${calleeMethod.methodIdentifier}")
          None
        } else {
          Some(getSummary(calleeMethod.methodIdentifier))
        }
      }
      .toSet
  }

}

object IFDSRunnerEnvironment {

  def apply(callGraph: CallGraph, summaryLookup: Map[MethodIdent, IFDSMethodGraph]): IFDSRunnerEnvironment =
    new IFDSRunnerEnvironment(callGraph, summaryLookup)

}
