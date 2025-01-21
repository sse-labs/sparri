package org.anon.spareuse.client.analyses.ifds

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.{CallStatementNode, IFDSFact, IFDSMethodGraph}
import org.anon.spareuse.execution.analyses.impl.ifds.reachability.IFDSRunnerEnvironment
import org.opalj.br.DeclaredMethod
import org.opalj.tac.cg.CallGraph

import scala.collection.mutable

/**
 * This custom IFDS runner environment can be used for modular analyses where we only want to find the set of valid
 * facts per library entry point. These entry points and facts are then stored to be queried at an oracle implementation.
 * @param cg The call graph for IFDS reachability analysis
 * @param summaryDict The summary map of all relevant methods
 * @param libEntryPoints The set of library entry points
 */
class ModularIFDSRunnerEnvironment(cg: CallGraph,
                                   summaryDict: Map[MethodIdent, IFDSMethodGraph],
                                   libEntryPoints: Set[MethodIdent]) extends IFDSRunnerEnvironment(null, summaryDict, analyzeJRE = false) {

  private val libEntryNodes: mutable.Map[MethodIdent, mutable.Set[(MethodIdent, Int)]] = mutable.HashMap.empty
  private val methodLookup: Map[MethodIdent, DeclaredMethod] =  cg
    .reachableMethods()
    .map{ ctx =>
      val ident = MethodIdent(ctx.method.declaringClassType.fqn, ctx.method.name, ctx.method.descriptor.toJVMDescriptor)
      (ident, ctx.method)
    }
    .toMap

  private val methodIdentLookup: mutable.Map[DeclaredMethod, MethodIdent] = mutable.Map.empty

  private def toIdent(declaredMethod: DeclaredMethod): MethodIdent = {
    if(methodIdentLookup.contains(declaredMethod)) methodIdentLookup(declaredMethod)
    else {
      val ident = MethodIdent(declaredMethod.declaringClassType.fqn, declaredMethod.name, declaredMethod.descriptor.toJVMDescriptor)
      methodIdentLookup(declaredMethod) = ident
      ident
    }
  }

  override def getTargetMethodSummaries(csn: CallStatementNode, caller: MethodIdent): Set[IFDSMethodGraph] = {

    // Get normal method targets based on OPAL CG
    val opalMethod = methodLookup(caller)
    val originalTargets = cg
      .calleesOf(opalMethod, csn.stmtPc)
      .map(ctx => toIdent(ctx.method))
      .toSet

    // For each lib entry point target we store the calling node that leads to that entry point
    originalTargets
      .filter(target => libEntryPoints.contains(target))
      .foreach { target =>
        if(!libEntryNodes.contains(target))
          libEntryNodes.put(target, mutable.Set((caller, csn.stmtPc)))
        else
          libEntryNodes(target).add((caller, csn.stmtPc))
      }

    // Return only those targets that are not entries to libraries
    // We only want to propagate facts along the client side of the code
    originalTargets
      .filterNot(libEntryPoints.contains)
      .filter(summaryDict.contains)
      .map(summaryDict)
      .toSet
  }

  def getAllQueries: Set[IFDSQuery] = {
    libEntryNodes
      .map{ case (libEntry, callNodes) =>
        val allFacts = callNodes.flatMap( nodeTuple => methodStatementsToFactMap(nodeTuple._1)(nodeTuple._2)).toSet
        IFDSQuery(libEntry, allFacts)
      }
      .toSet
  }
}

case class IFDSQuery(ident: MethodIdent, factsAtEntry: Set[IFDSFact])
