package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.TaintVariableFacts.TaintFunctionReturn
import org.anon.spareuse.execution.analyses.{buildProject, getTACProvider, loadFixture}
import org.scalatest.funspec.AnyFunSpec

class IFDSTaintFlowSummaryBuilderImplTest extends AnyFunSpec {

  describe("The IFDS Taint Flow Summary Builder"){

    it("should keep track of taint across field stores and reads") {
      val graphs = getMethodSummariesFromFixture("SimpleStringTaint.class", Set.empty)

      var foundSink = false

      def findSink(sn: StatementNode, currentFacts: Set[IFDSFact]): Unit = {
        sn match {
          case csn: CallStatementNode if csn.functionName == "sink" =>
            foundSink = true
            assert(csn.parameterVariables.size == 1)
            val theParam = csn.parameterVariables.head
            val paramFact = TaintVariableFacts.buildFact(theParam)
            assert(!currentFacts.exists(f => f.uniqueIdent == paramFact.uniqueIdent)) // Param cannot be tainted here, as it is dependent on an (unknown) function return
          case _ =>
        }

        val newFacts = sn.getFactsAfter(currentFacts)

        sn.stmtPc match {
          case 0 =>
            assert(newFacts.size == 2)
            val newFact = newFacts.diff(currentFacts).head
            assert(newFact.displayName == "lv0")
          case 4 =>
            assert(newFacts.size == 2)
          case 9 =>
            assert(sn.allActivations.size == 1)
            val activation = sn.allActivations.head
            assert(activation._1.displayName == "lv2")
            assert(activation._2.size == 1)
            val activatesOn = activation._2.head
            assert(activatesOn.isInstanceOf[TaintFunctionReturn]) // Assert that the graph holds an artificial method return fact
          case _ =>
        }

        sn.getSuccessors.foreach(s => findSink(s, newFacts))
      }

      val mainGraph = graphs.find(_.methodName == "main").get
      assert(mainGraph.hasStatement(0))
      findSink(mainGraph.getStatement(0).get, Set(IFDSZeroFact))
      assert(foundSink)


    }

    it("should handle String additions correctly"){

      val concatGraph = getMethodSummariesFromFixture("StringConcatenation.class", Set("$string_concat$add")).head

      assert(concatGraph.statementNodes.nonEmpty)

      concatGraph.print()

      val param1Opt = concatGraph.allFacts.find(_.displayName == "param1")
      val param2Opt = concatGraph.allFacts.find(_.displayName == "param2")
      val returnOpt = concatGraph.statementNodes.find(_.isReturnValue).flatMap(_.asReturnNode.variableReturned)

      assert(param1Opt.isDefined && param2Opt.isDefined && returnOpt.isDefined)
      val returnFact = TaintVariableFacts.buildFact(returnOpt.get)


      def yieldsTaintedReturn(initialFacts: Set[IFDSFact]):  Boolean = {
        var currFacts = initialFacts
        concatGraph.statementNodes.foreach{ snode =>
          currFacts = snode.getFactsAfter(currFacts)
        }

        currFacts.contains(returnFact)
      }

      val emptyTargetProvider: MethodIdent => Int => Set[IFDSMethodGraph] = {_ => {_ => Set.empty}}

      // Assert that one tainted parameter in a + b yields tainted result
      val param1 = param1Opt.get
      assert(yieldsTaintedReturn(Set(IFDSZeroFact, param1)))
      assert(concatGraph.runWith(Set(IFDSZeroFact, param1))(emptyTargetProvider).contains(returnFact))

      val param2 = param2Opt.get
      assert(yieldsTaintedReturn(Set(IFDSZeroFact, param2)))
      assert(concatGraph.runWith(Set(IFDSZeroFact, param2))(emptyTargetProvider).contains(returnFact))

      // Assert that both tainted parameters yield a tainted result
      assert(yieldsTaintedReturn(Set(IFDSZeroFact, param1, param2)))
      assert(concatGraph.runWith(Set(IFDSZeroFact, param1, param2))(emptyTargetProvider).contains(returnFact))

      // Assert that no tainted parameter yields untainted result
      assert(! yieldsTaintedReturn(Set(IFDSZeroFact)))
      assert(! concatGraph.runWith(Set(IFDSZeroFact))(emptyTargetProvider).contains(returnFact))

    }

    it("should handle invocations of String additions correctly"){
      val graphs = getMethodSummariesFromFixture("StringConcatenation.class", Set("add", "$string_concat$add"))
      val addGraph = graphs.find(_.methodName == "add").get
      val concatGraph = graphs.find(_.methodName.startsWith("$")).get

      def callTargetProvider(mi: MethodIdent): Int => Set[IFDSMethodGraph] = mi match {
        case MethodIdent(addGraph.methodIdentifier.declaredType, addGraph.methodIdentifier.methodName, addGraph.methodIdentifier.methodDescriptor) =>
          {
            case 7 => Set(concatGraph)
            case _ => Set.empty
          }
        case _ =>
           _ => Set.empty

      }

      assert(addGraph.statementNodes.nonEmpty)

      addGraph.print()

      val param1Opt = concatGraph.allFacts.find(_.displayName == "param1")
      val param2Opt = concatGraph.allFacts.find(_.displayName == "param2")
      val returnFactOpt = addGraph.statementNodes.find(_.isReturnValue).flatMap(_.asReturnNode.variableReturned).map(TaintVariableFacts.buildFact)

      assert(param1Opt.isDefined && param2Opt.isDefined && returnFactOpt.isDefined)

      val param1 = param1Opt.get
      val param2 = param2Opt.get
      val returnFact = returnFactOpt.get

      // If no parameter is tainted, then the result should not be tainted
      val noTaintResult = addGraph.runWith(Set(IFDSZeroFact))(callTargetProvider)
      assert(!noTaintResult.contains(returnFact))

      // If the first parameter is tainted, then the result should be tainted
      val oneTaintResult = addGraph.runWith(Set(IFDSZeroFact, param1))(callTargetProvider)
      assert(oneTaintResult.contains(returnFact))

      // If the second parameter is tainted, the result should not be tainted - it is overridden
      val twoTaintResult = addGraph.runWith(Set(IFDSZeroFact, param2))(callTargetProvider)
      assert(! twoTaintResult.contains(returnFact))

      // If both parameters are tainted, the result should be tainted
      val bothTaintResult = addGraph.runWith(Set(IFDSZeroFact, param1, param2))(callTargetProvider)
      assert(bothTaintResult.contains(returnFact))

    }

  }

  private def getMethodSummariesFromFixture(fixtureName: String, methodNames: Set[String]): Set[IFDSMethodGraph] = {
    val project = buildProject(loadFixture(fixtureName))
    val tacProvider = getTACProvider(project)

    val relevantMethods = if(methodNames.isEmpty) project.allMethodsWithBody else project.allMethodsWithBody.filter(m => methodNames.exists(s => m.name.startsWith(s)))

    assert(relevantMethods.nonEmpty)

    val ifdsBuilder = new IFDSTaintFlowSummaryBuilderImpl(None)
    val graphs = relevantMethods.map(m => ifdsBuilder.analyzeMethod(m)(tacProvider)).toSet

    assert(graphs.size == relevantMethods.size)

    graphs
  }

}
