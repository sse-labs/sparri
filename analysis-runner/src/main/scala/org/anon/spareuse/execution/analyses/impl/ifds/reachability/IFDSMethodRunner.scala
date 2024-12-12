package org.anon.spareuse.execution.analyses.impl.ifds.reachability

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.MethodIFDSRep
import org.anon.spareuse.execution.analyses.impl.ifds.IFDSMethodGraph

import scala.util.{Failure, Success, Try}

class IFDSMethodRunner private(environment: IFDSRunnerEnvironment){

}

object IFDSMethodRunner {
  private[reachability] type CallGraph = CallGraphBuilder#CallGraphView

  def apply(cg: CallGraph, summaryLookup: Map[MethodIdent, MethodIFDSRep]): IFDSMethodRunner = {
    val convertedLookup = Try( summaryLookup.view.mapValues(IFDSMethodGraph.apply).toMap )

    convertedLookup match {
      case Success(lookup) =>
        new IFDSMethodRunner(IFDSRunnerEnvironment(cg, lookup))
      case Failure(ex) =>
        throw new RuntimeException("Failed to convert IFDS summary to object representation", ex)
    }
  }
}
