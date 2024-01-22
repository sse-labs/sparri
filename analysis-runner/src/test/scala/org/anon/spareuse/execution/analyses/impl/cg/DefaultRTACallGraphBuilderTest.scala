package org.anon.spareuse.execution.analyses.impl.cg

import org.scalatest.funspec.AnyFunSpec

class DefaultRTACallGraphBuilderTest extends AnyFunSpec with CallGraphTestSupport {

  describe("The default RTA CG builder"){

    it("should correctly resolve some virtual calls without the JRE present"){
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), None)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(_.name == "doVirtualCalls").get)

      val result = builder.buildFrom(sourceDm)
      assert(result.isSuccess)
      assert(result.get.reachableMethods().nonEmpty)

      val calls_doStatic = builder.asDefinedMethod(input.allMethods.find(m => m.name == "doStaticCalls" && m.enclosingClass.get.thisType == "Calls").get)
      val callTargetImpl_doStatic = builder.asDefinedMethod(input.allMethods.find(m => m.name == "doStaticCalls" && m.enclosingClass.get.thisType == "CallTargetImpl").get)

      assert(builder.calleeMap.contains(sourceDm))
      val resolutions = builder.calleeMap(sourceDm)

      // Three INVOKEVIRTUALs (toString should not resolve here) and four INVOKESPECIALs
      assert(resolutions.size == 7)

      // Assert that all four INVOKESPECIALs are resolved to exactly one method (the constructor) on the correct type
      assert(resolutions.contains(5) && resolutions(5).size == 1 && resolutions(5).head.definingTypeName == "BranchingTaint")
      assert(resolutions.contains(23) && resolutions(23).size == 1 && resolutions(23).head.definingTypeName == "Calls")
      assert(resolutions.contains(35) && resolutions(35).size == 1 && resolutions(35).head.definingTypeName == "CallTargetImpl")
      assert(resolutions.contains(47) && resolutions(47).size == 1 && resolutions(47).head.definingTypeName == "CallTargetImpl")

      // Assert that the three resolvable invocations resolve to the correct targets
      assert(resolutions.contains(28))
      assert(resolutions(28).size == 2)
      assert(resolutions(28).contains(calls_doStatic) && resolutions(28).contains(callTargetImpl_doStatic))

      assert(resolutions.contains(40))
      assert(resolutions(40).size == 2)
      assert(resolutions(40).contains(calls_doStatic) && resolutions(40).contains(callTargetImpl_doStatic))

      assert(resolutions.contains(54))
      assert(resolutions(54).size == 1)
      assert(resolutions(54).contains(callTargetImpl_doStatic))
    }

    it("should handle recursion correctly") {
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), None)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(dm => dm.name == "doRecursiveCalls" && dm.enclosingClass.get.thisType == "Calls").get)

      val result = builder.buildFrom(sourceDm)

      assert(result.isSuccess)
      assert(result.get.reachableMethods().nonEmpty)

      assert(builder.calleeMap.contains(sourceDm))
      val res1 = builder.calleeMap(sourceDm)
      assert(res1.contains(4) && res1.contains(10))
      assert(res1(10).size == 2)
      assert(res1(10).exists(_.definingTypeName == "Calls"))
      assert(res1(10).exists(_.definingTypeName == "CallTargetImpl"))

      val recurseSuper = builder.calleeMap.keys.find(dm => dm.methodName == "recurse" && dm.definingTypeName == "Calls").get
      assert(builder.calleeMap.contains(recurseSuper))
      val res2 = builder.calleeMap(recurseSuper)
      assert(res2.size == 3)
      assert(res2.contains(1) && res2.contains(8) && res2.contains(13))
      assert(res2(1).size == 2)
      assert(res2(13).size == 2)
    }

    it("should resolve calls that depend on instantiations in other methods"){
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), None)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(dm => dm.name == "doReturnDependent" && dm.enclosingClass.get.thisType == "Calls").get)

      assert(builder.buildFrom(sourceDm).isSuccess)

      assert(builder.calleeMap(sourceDm).size == 2)

      val resolutions = builder.calleeMap(sourceDm)
      assert(resolutions.contains(1) && resolutions.contains(6))
      assert(resolutions(1).size == 1 && resolutions(1).head.definingTypeName == "Calls")
      assert(resolutions(6).exists(_.definingTypeName == "Calls"))
      assert(resolutions(6).exists(_.definingTypeName == "CallTargetImpl")) // This is the important one! Capture that an initialization in a previous method leads to a new instantiable type here
      assert(resolutions(6).size == 2)
    }

    it("should resolve all virtual calls with the JRE present"){
      resetModelLoader()
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(_.name == "doVirtualCalls").get)

      val result = builder.buildFrom(sourceDm)
      assert(result.isSuccess)

      // There should now be a resolution for Object.toString at PC 15
      assert(builder.calleeMap.contains(sourceDm) && builder.calleeMap(sourceDm).size == 8)
      assert(builder.calleeMap(sourceDm).contains(15))

      // There could be multiple resolutions, but java/lang/Object.toString must be contained
      val toStringResolutions = builder.calleeMap(sourceDm)(15)
      assert(toStringResolutions.nonEmpty && toStringResolutions.exists(_.definingTypeName == objFqn))

      val allCallSitesCount = builder.calleeMap.values.map(callSites => callSites.size).sum
      val allTargetsCount = builder.calleeMap.values.map(callSites => callSites.values.map(_.size).sum).sum

      println(s"Found ${result.get.reachableMethods().size} reachable methods.")
      println(s"Resolved $allCallSitesCount callsites to $allTargetsCount methods")
    }

  }

}
