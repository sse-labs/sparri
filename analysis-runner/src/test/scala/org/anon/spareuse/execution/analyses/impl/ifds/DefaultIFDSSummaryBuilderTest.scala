package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.AnalysisRunData
import org.anon.spareuse.execution.analyses.{AnalysisImplementationDescriptor, buildProject, getTACProvider, loadFixture}
import org.opalj.br.Method
import org.opalj.tac.{If, Return}
import org.scalatest.funspec.AnyFunSpec

class DefaultIFDSSummaryBuilderTest extends AnyFunSpec {

  describe("Any default IFDS Summary Builder") {

    it("should build valid graphs for linear methods without any facts involved"){
      val project = buildProject(loadFixture("SimpleStringTaint.class"))
      val tacProvider = getTACProvider(project)
      val allMethods = project.allMethodsWithBody.filterNot(m => m.toJava.contains("init"))

      assert(allMethods.size == 3)
      var stmtCnt = 0

      val ifdsBuilder = createDummyBuilder(None){ (node, _, _) =>
        println(node.stmtRep)
        stmtCnt += 1
      }

      val graphs = allMethods.map(m => ifdsBuilder.analyzeMethod(m)(tacProvider))

      // There are ten statements in three methods of this fixture
      assert(stmtCnt == 10)

      // For an empty builder, no facts will be present in the graph (except the zero fact)
      assert(graphs.forall(g => g.allFacts.size == 1 && g.allFacts.head == IFDSZeroFact))

      // All graphs should at least contain a return statement
      assert(graphs.forall(g => g.statementNodes.nonEmpty))

      // Main method graph must be present
      val mainMethodOpt = graphs.find(g => g.methodName == "main")
      assert(mainMethodOpt.isDefined)

      // Main method graph must have 7 statements
      val mainGraph = mainMethodOpt.get
      assert(mainGraph.statementNodes.size == 7)
      assert(mainGraph.hasStatement(0))

      // First statement of this fixture's main method is a assignment
      val firstStmt = mainGraph.getStatement(0).get
      assert(firstStmt.stmtRep.startsWith("Assignment("))

      // The main method has no branches at all, so all statements have exactly one successor and one / zero predecessors
      var currNode = firstStmt
      while(!currNode.stmtRep.startsWith("Return(")){
        assert(currNode.getSuccessors.size == 1)
        assert(currNode.getPredecessors.size <= 1)
        currNode = currNode.getSuccessors.head
      }

      // Assert that all three calls are present in the fixture's main method
      val callNodes = mainGraph.statementNodes.filter(_.isCallNode)
      assert(callNodes.size == 3)
      assert(callNodes.exists(sn => sn.asCallNode.declaringClassFqn.endsWith("SimpleStringTaint") && sn.asCallNode.functionName == "source"))
      assert(callNodes.exists(sn => sn.asCallNode.declaringClassFqn.endsWith("SimpleStringTaint") && sn.asCallNode.functionName == "sink"))

      // Assert that the static call to "concatStrings" has no receiver and two actual parameters
      val concatCallOpt = callNodes.find(sn => sn.asCallNode.declaringClassFqn.endsWith("StringConcatHelper") && sn.asCallNode.functionName == "concatStrings")
      assert(concatCallOpt.isDefined)
      val concatCall = concatCallOpt.get.asCallNode
      assert(concatCall.receiver.isEmpty)
      assert(concatCall.parameterVariables.size == 2)

      // The source method of this fixture must result in a graph
      val sourceMethodOpt = graphs.find(g => g.methodName == "source")
      assert(sourceMethodOpt.isDefined)

      // The source method has two statements, one is a assignment and one a value return
      val sourceGraph = sourceMethodOpt.get
      assert(sourceGraph.statementNodes.size == 2)
      assert(sourceGraph.hasStatement(0))
      assert(sourceGraph.getStatement(0).get.getPredecessors.isEmpty && sourceGraph.getStatement(0).get.getSuccessors.size == 1 )//&& sourceGraph.getStatement(0).get.stmt.isAssignment)
      val secondStmt = sourceGraph.getStatement(0).get.getSuccessors.head
      assert(secondStmt.getPredecessors.size == 1 && secondStmt.getSuccessors.isEmpty && secondStmt.isReturnValue)
      assert(secondStmt.asInstanceOf[ReturnValueStatementNode].variableReturned.isDefined)
      // Graphs must correctly keep track of value returns
      assert(sourceGraph.isReturnNode(secondStmt.stmtPc))
    }

    it("should build valid cfgs for code containing branches and loops"){
      val project = buildProject(loadFixture("BranchingTaint.class"))
      val tacProvider = getTACProvider(project)
      val allMethods = project.allMethodsWithBody.filterNot(m => m.toJava.contains("init"))

      assert(allMethods.size == 3)

      val ifdsBuilder = createDummyBuilder(None) { (node, _, _) =>
        println(node.stmtRep)
      }

      val mainGraph = ifdsBuilder.analyzeMethod(allMethods.find(_.name == "main").get)(tacProvider)
      assert(mainGraph.hasStatement(0))
      val firstStmt = mainGraph.getStatement(0).get
      assert(firstStmt.stmtRep.startsWith("Assignment("))

      var currNode = firstStmt
      while(currNode.stmtPc != 14){
        assert(currNode.getPredecessors.size <= 1)
        assert(currNode.getSuccessors.size == 1)
        currNode = currNode.getSuccessors.head
      }

      // Make sure there is an if statement at PC 14
      assert(currNode.stmtRep.startsWith("If("))
      // An IF must have two successors
      assert(currNode.getSuccessors.size == 2)
      assert(currNode.getSuccessors.exists(_.stmtPc == 17))
      assert(currNode.getSuccessors.exists(_.stmtPc == 23))

      // Make sure that both branches have two statements each, and then merge back into the same statement
      val b1 = mainGraph.getStatement(17).get
      val b2 = mainGraph.getStatement(23).get
      assert(b1.getSuccessors.size == 1)
      assert(b2.getSuccessors.size == 1)
      val s1 = b1.getSuccessors.head
      val s2 = b2.getSuccessors.head
      assert(s1.getSuccessors.size == 1 && s2.getSuccessors.size == 1)
      assert(s1.getSuccessors.head == s2.getSuccessors.head)

      // Move to start of loop
      currNode = s1.getSuccessors.head
      while(currNode.stmtPc != 31){
        assert(currNode.getPredecessors.nonEmpty)
        assert(currNode.getSuccessors.size == 1)
        currNode = currNode.getSuccessors.head
      }

      // Make sure there is an if statement at PC 31, ie start of the loop
      assert(currNode.stmtRep.startsWith("If("))
      // An IF must have two successors
      assert(currNode.getSuccessors.size == 2)
      assert(currNode.getPredecessors.head.getPredecessors.size == 2) // Two statements lead to the loop head
      assert(currNode.getSuccessors.exists(_.stmtPc == 37))
      assert(currNode.getSuccessors.exists(_.stmtPc == 44))
    }

    it("should keep track of facts and their activations"){

      class PcFact(val pc: Int) extends IFDSFact {
        override def uniqueIdent: String = s"RANDOM-$pc"

        override def displayName: String = s"<R> $pc"
      }

      val project = buildProject(loadFixture("SimpleStringTaint.class"))
      val tacProvider = getTACProvider(project)
      val allMethods = project.allMethodsWithBody.filterNot(m => m.toJava.contains("init"))

      assert(allMethods.size == 3)

      // Nonsensical way to create some dummy facts
      val ifdsBuilder = createDummyBuilder(None) { (node, _, graph) =>
        if(node.stmtPc % 2 == 0){
          node.setGeneratesFact(new PcFact(node.stmtPc))
        }

        val allFacts = graph.allFacts

        if (allFacts.nonEmpty) {
          node.setGeneratesOn(new PcFact(node.stmtPc), Set(allFacts.head))
        }
      }

      val graphs = allMethods.map(m => ifdsBuilder.analyzeMethod(m)(tacProvider))

      val mainGraph = graphs.find(_.methodName == "main").get
      mainGraph.print()

      assert(mainGraph.allFacts.size == 8)
      mainGraph
        .statementNodes
        .filter(_.stmtPc % 2 == 0)
        .foreach{ sn =>
          val key = new PcFact(sn.stmtPc)
          val activatesOn = sn.activatesOn(key)
          assert(activatesOn.nonEmpty && activatesOn.contains(IFDSZeroFact))

          if(activatesOn.size == 2 || sn.stmtPc % 2 == 1) assert(activatesOn.contains(new PcFact(0)))
        }
    }

  }






  private def createDummyBuilder(brOpt: Option[AnalysisRunData],
                                 aName: String = "Demo",
                                 aVersion: String = "1.0.0")(impl: (StatementNode, Method, IFDSMethodGraph) => Unit): DefaultIFDSSummaryBuilder =

  new DefaultIFDSSummaryBuilder(brOpt) {
    protected val analysisName: String = aName
    protected val analysisVersion: String = aVersion
    protected val analysisDescription: String = "Demo builder for test purposes"

    override val descriptor: AnalysisImplementationDescriptor = DefaultIFDSSummaryBuilder.buildDescriptor(analysisName, analysisVersion, analysisDescription)

    override protected[ifds] def analyzeStatement(currentNode: StatementNode, currentStmt: TACStmt, currentMethod: Method, graph: IFDSMethodGraph): Unit = impl(currentNode, currentMethod, graph)
  }

}
