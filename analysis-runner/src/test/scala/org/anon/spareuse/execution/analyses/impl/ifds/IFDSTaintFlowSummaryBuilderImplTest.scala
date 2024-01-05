package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.execution.analyses.impl.ifds.TaintVariableFacts.TaintFunctionReturn
import org.anon.spareuse.execution.analyses.{buildProject, getTACProvider, loadFixture}
import org.opalj.br.ObjectType
import org.scalatest.funspec.AnyFunSpec

class IFDSTaintFlowSummaryBuilderImplTest extends AnyFunSpec {

  describe("The IFDS Taint Flow Summary Builder"){

    it("should keep track of taint across field stores and reads") {
      val project = buildProject(loadFixture("SimpleStringTaint.class"))
      val tacProvider = getTACProvider(project)
      val allMethods = project.allMethodsWithBody.filterNot(m => m.toJava.contains("init"))

      val ifdsBuilder = new IFDSTaintFlowSummaryBuilderImpl(None)

      val graphs = allMethods.map(m => ifdsBuilder.analyzeMethod(m)(tacProvider))

      var foundSink = false

      def findSink(sn: StatementNode, currentFacts: Set[IFDSFact]): Unit = {
        sn match {
          case csn: CallStatementNode if csn.functionName == "sink" =>
            foundSink = true
            assert(csn.parameterVariables.size == 1)
            val theParam = csn.parameterVariables.head
            val paramFact = TaintVariableFacts.buildFact(theParam)
            assert(!currentFacts.contains(paramFact)) // Param cannot be tainted here, as it is dependent on an (unknown) function return
          case _ =>
        }

        val newFacts = sn.getFactsAfter(currentFacts)

        sn.stmt.pc match {
          case 0 =>
            val targetFact = TaintVariableFacts.buildFact(sn.stmt.asAssignment.targetVar)
            assert(newFacts.contains(targetFact)) //PC=0 taints the first local variable, as it assigns "source" to it
            assert(newFacts.size == 2)
          case 4 =>
            assert(newFacts.size == 2)
          case 9 =>
            val targetFact = TaintVariableFacts.buildFact(sn.stmt.asAssignment.targetVar)
            assert(sn.hasActivation(targetFact))
            val a = sn.activatesOn(targetFact)
            assert(a.size == 1 && a.head.isInstanceOf[TaintFunctionReturn]) // Assert that the graph holds an artificial method return fact
          case _ =>
        }

        sn.getSuccessors.foreach(s => findSink(s, newFacts))
      }

      val mainGraph = graphs.find(_.method.name == "main").get
      assert(mainGraph.hasStatement(0))
      findSink(mainGraph.getStatement(0).get, Set(IFDSZeroFact))
      assert(foundSink)


    }

  }

}
