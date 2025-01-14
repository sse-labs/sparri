package org.anon.spareuse.execution.analyses.impl.ifds.reachability

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.{allFixtureNames, getAllFixturesProject, getMethodSummariesFromFixture, toObjectModel}
import org.anon.spareuse.execution.analyses.impl.cg.{CallGraphTestSupport, DefaultRTACallGraphBuilder, JreModelLoader}
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSMethodGraph, IFDSZeroFact}
import org.scalatest.funspec.AnyFunSpec

class IFDSMethodRunnerTest extends AnyFunSpec with CallGraphTestSupport {

  // Dummy object representing all fixture classes as if they were a coherent program
  private lazy val fixtureProgram: JavaProgram = toObjectModel(getAllFixturesProject)

  private lazy val allSummaries: Set[IFDSMethodGraph] = allFixtureNames.flatMap(fixture => getMethodSummariesFromFixture(fixture, Set())).toSet

  describe("The IFDS Method Runner") {

    it("should work for empty fact lists") {
      resetModelLoader()
      val (runner, entryMethodIdent) = buildRunnerFor("main", "BranchingTaint")

      val resultingFacts = runner.resolveFrom(entryMethodIdent, Set(IFDSZeroFact))

      assert(resultingFacts.nonEmpty)
      assert(resultingFacts.contains(IFDSZeroFact))
    }

    it("should work with different types of invocations") {
      resetModelLoader()
      val (runner, entryMethodIdent) = buildRunnerFor("doVirtualCalls", "Calls")

      val resultingFacts = runner.resolveFrom(entryMethodIdent, Set(IFDSZeroFact))

      assert(resultingFacts.size == 1)
      assert(resultingFacts.head == IFDSZeroFact)
    }

    it("should work on loops and conditionals") {
      resetModelLoader()
      val (runner, entryMethodIdent) = buildRunnerFor("main", "BranchingTaint")

      val resultingFacts = runner.resolveFrom(entryMethodIdent, Set(IFDSZeroFact))

      val lv0Variable = runner.environment.getSummary(entryMethodIdent).getEntryBlock.get.activations.keys.head

      // This method automatically taints the lv0 variable
      assert(resultingFacts.contains(lv0Variable))
      assert(resultingFacts.contains(IFDSZeroFact))
    }

    it("should correctly propagate taint for string concatenation") {
      assertOneOrMoreParamsTaintResult("concatStrings", "StringConcatHelper")
    }

    it("should handle addition of strings correctly"){
      assertOneOrMoreParamsTaintResult("add", "StringConcatenation")
    }

    it("should handle initializations of StringBuilders correctly") {
      assertOneParamTaintsResults("initTaint", "StringConcatenation")
    }

    it("should handle replacements correctly") {
      assertOneParamTaintsResults("replace", "StringConcatenation")
    }

    it("should handle insertions correctly") {
      assertOneParamTaintsResults("insert", "StringConcatenation")
    }

    it("should handle aliasing correctly") {
      assertOneParamTaintsResults("alias", "StringConcatenation")
    }

    it("should handle transitive aliasing correctly") {
      assertOneParamTaintsResults("transitiveAlias", "StringConcatenation")
    }

  }

  def assertOneParamTaintsResults(methodName: String, methodClass: String): Unit = {
    assertTaintPropagation(methodName, methodClass, Set.empty, resultShouldBeTainted = false)
    assertTaintPropagation(methodName, methodClass, Set(0), resultShouldBeTainted = true)
  }

  def assertOneOrMoreParamsTaintResult(methodName: String, methodClass: String): Unit = {
    assertTaintPropagation(methodName, methodClass, Set.empty, resultShouldBeTainted = false)
    assertTaintPropagation(methodName, methodClass, Set(0), resultShouldBeTainted = true)
    assertTaintPropagation(methodName, methodClass, Set(1), resultShouldBeTainted = true)
    assertTaintPropagation(methodName, methodClass, Set(0,1), resultShouldBeTainted = true)
  }

  def assertTaintPropagation(methodName: String, methodClass: String, paramIdxTainted: Set[Int], resultShouldBeTainted: Boolean): Unit = {
    resetModelLoader()
    val (runner, entryMethodIdent) = buildRunnerFor(methodName, methodClass)

    val concatMethodSummary = runner.environment.getSummary(entryMethodIdent)

    assert(concatMethodSummary.allVariablesReturned.size == 1)

    val extraFacts = paramIdxTainted.map(idx =>  concatMethodSummary.parameterFacts.find(_.parameterIdx == idx)).filter(_.isDefined).map(_.get)
    val returnFact = concatMethodSummary.allVariablesReturned.head

    val resultingFacts = runner.resolveFrom(entryMethodIdent, Set(IFDSZeroFact) ++ extraFacts)

    assert(extraFacts.isEmpty || runner.environment.methodReturnIsTainted(entryMethodIdent) == resultShouldBeTainted)
    assert(extraFacts.isEmpty || resultingFacts.contains(returnFact) == resultShouldBeTainted)
  }

  private def buildRunnerFor(entryMethodName: String, entryMethodClassName: String): (IFDSMethodRunner, MethodIdent) = {
    val cgBuilder = new DefaultRTACallGraphBuilder(Set(fixtureProgram), None)
    val entryMethod = cgBuilder.asDefinedMethod(fixtureProgram.allMethods.find(m => m.name == entryMethodName && m.enclosingClass.get.thisType == entryMethodClassName).get)

    val cgResult = cgBuilder.buildFrom(entryMethod)

    assert(cgResult.isSuccess)

    (IFDSMethodRunner(cgResult.get, allSummaries.map(s => (s.methodIdentifier, s.toResultRepresentation(true))).toMap), entryMethod.methodIdentifier)
  }


}
