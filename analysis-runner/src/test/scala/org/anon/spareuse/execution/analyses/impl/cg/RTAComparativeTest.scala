package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.execution.analyses.getCallGraphProjectWithJre
import org.opalj.tac.cg.RTACallGraphKey
import org.scalatest.funspec.AnyFunSpec

import scala.util.{Failure, Success}

class RTAComparativeTest extends AnyFunSpec with CallGraphTestSupport {

  describe("The naive RTA implementation of SPARRI") {
    it("should produce results in a given timeframe"){
      resetModelLoader()
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))
      assert(builder.jreOpt.isDefined)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(dm => dm.name == "main" && dm.enclosingClass.get.thisType == "BranchingTaint").get)

      val start = System.currentTimeMillis()
      builder.buildFrom(sourceDm) match {
        case Failure(ex) => fail(ex)
        case Success(cg) =>
          val time = (System.currentTimeMillis() - start) / 1000

          println(s"Found ${cg.reachableMethods().size} reachable methods in $time seconds.")
      }

    }

  }

  describe("The default RTA implementation"){

    it("should produce results in a given timeframe"){
      resetModelLoader()
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))
      assert(builder.jreOpt.isDefined)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(dm => dm.name == "main" && dm.enclosingClass.get.thisType == "BranchingTaint").get)

      val start = System.currentTimeMillis()
      builder.buildFrom(sourceDm) match {
        case Failure(ex) => fail(ex)
        case Success(cg) =>
          val time = (System.currentTimeMillis() - start) / 1000

          println(s"Found ${cg.reachableMethods().size} reachable methods in $time seconds.")
      }
    }

    it("should compare to the OPAL performance") {
      val start = System.currentTimeMillis()
      val opalProject = getCallGraphProjectWithJre
      val cg = opalProject.get(RTACallGraphKey)

      val time = (System.currentTimeMillis() - start) / 1000
      println(s"OPAL found ${cg.reachableMethods().size} reachable methods in $time seconds")
    }
  }

}
