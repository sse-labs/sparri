package org.anon.spareuse.execution.analyses.impl.ifds.reachability

import org.anon.spareuse.execution.analyses.getMethodSummariesFromFixture
import org.anon.spareuse.execution.analyses.impl.cg.{CallGraphTestSupport, DefaultRTACallGraphBuilder, JreModelLoader}
import org.anon.spareuse.execution.analyses.impl.ifds.IFDSZeroFact
import org.scalatest.funspec.AnyFunSpec

class IFDSMethodRunnerTest extends AnyFunSpec with CallGraphTestSupport {

  describe("The IFDS Method Runner") {
    it("should work for empty fact lists") {
      resetModelLoader()
      val graphs = getMethodSummariesFromFixture("BranchingTaint.class", Set("main", "source", "sink"))
      val program = getCgFixtureModel

      val builder = new DefaultRTACallGraphBuilder(Set(program), None)

      val sourceDm = builder.asDefinedMethod(program.allMethods.find(m => m.name == "main" && m.enclosingClass.exists(_.thisType == "BranchingTaint")).get)

      val result = builder.buildFrom(sourceDm)
      assert(result.isSuccess)

      val runner = IFDSMethodRunner(result.get, graphs.map(graph => (graph.methodIdentifier, graph.toResultRepresentation(true))).toMap)

      val resultingFacts = runner.resolveFrom(sourceDm.methodIdentifier, Set(IFDSZeroFact))

      assert(resultingFacts.nonEmpty)
      assert(resultingFacts.contains(IFDSZeroFact))
    }
  }


}
