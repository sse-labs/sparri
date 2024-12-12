package org.anon.spareuse.execution.analyses.impl.ifds.reachability

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.IFDSMethodGraph
import org.anon.spareuse.execution.analyses.impl.ifds.reachability.IFDSMethodRunner.CallGraph

class IFDSRunnerEnvironment private(cg: CallGraph, summaryLookup: Map[MethodIdent, IFDSMethodGraph]) {

}

object IFDSRunnerEnvironment {

  def apply(callGraph: CallGraph, summaryLookup: Map[MethodIdent, IFDSMethodGraph]): IFDSRunnerEnvironment =
    new IFDSRunnerEnvironment(callGraph, summaryLookup)

}
