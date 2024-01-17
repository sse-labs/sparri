package org.anon.spareuse.execution.analyses.impl.cg

import org.scalatest.funspec.AnyFunSpec

class DefaultRTACallGraphBuilderTest extends AnyFunSpec with CallGraphTestSupport {

  describe("The default RTA CG builder"){

    it("should correctly resolve some virtual calls without the JRE present"){
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), None)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(_.name == "doVirtualCalls").get)

      val result = builder.resolveFrom(sourceDm)
      assert(result.isSuccess)
      assert(result.get.nonEmpty)

      val calls_doStatic = builder.asDefinedMethod(input.allMethods.find(m => m.name == "doStaticCalls" && m.getEnclosingClass.get.thisType == "Calls").get)
      val callTargetImpl_doStatic = builder.asDefinedMethod(input.allMethods.find(m => m.name == "doStaticCalls" && m.getEnclosingClass.get.thisType == "CallTargetImpl").get)

      assert(builder.calleesOf.contains(sourceDm))
      val resolutions = builder.calleesOf(sourceDm)

      // Three INVOKEVIRTUALs (toString should not resolve here) and four INVOKESPECIALs
      assert(resolutions.size == 7)

      // Assert that all four INVOKESPECIALs are resolved to exactly one method (the constructor) on the correct type
      assert(resolutions.contains(5) && resolutions(5).size == 1 && resolutions(5).head.definingTypeName == "BranchingTaint")
      assert(resolutions.contains(23) && resolutions(23).size == 1 && resolutions(23).head.definingTypeName == "Calls")
      assert(resolutions.contains(35) && resolutions(35).size == 1 && resolutions(35).head.definingTypeName == "CallTargetImpl")
      assert(resolutions.contains(47) && resolutions(47).size == 1 && resolutions(47).head.definingTypeName == "CallTargetImpl")

      // Assert that the three resolvable invocations resolve to the correct targets
      assert(resolutions.contains(28))
      assert(resolutions(28).size == 1)
      assert(resolutions(28).contains(calls_doStatic))

      assert(resolutions.contains(40))
      assert(resolutions(40).size == 2)
      assert(resolutions(40).contains(calls_doStatic) && resolutions(40).contains(callTargetImpl_doStatic))

      assert(resolutions.contains(54))
      assert(resolutions(54).size == 1)
      assert(resolutions(54).contains(callTargetImpl_doStatic))
    }

    it("should resolve all virtual calls with the JRE present"){
      resetModelLoader()
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(_.name == "doVirtualCalls").get)

      val result = builder.resolveFrom(sourceDm)
      assert(result.isSuccess)
      assert(result.get.nonEmpty)

      // There should now be a resolution for Object.toString at PC 15
      assert(builder.calleesOf.contains(sourceDm) && builder.calleesOf(sourceDm).size == 8)
      assert(builder.calleesOf(sourceDm).contains(15))

      // The only resolution should be to java/lang/Object.toString, as no other subclass is instantiated and contains a toString definition
      val toStringResolutions = builder.calleesOf(sourceDm)(15)
      assert(toStringResolutions.size == 1 && toStringResolutions.head.definingTypeName == objFqn)

      val allCallSitesCount = builder.calleesOf.values.map(callSites => callSites.size).sum
      val allTargetsCount = builder.calleesOf.values.map(callSites => callSites.values.map(_.size).sum).sum

      println(s"Found ${builder.reachableMethods().size} reachable methods.")
      println(s"Resolved $allCallSitesCount callsites to $allTargetsCount methods")
    }

    it("should handle recursion correctly"){
      val input = getCgFixtureModel
      val builder = new DefaultRTACallGraphBuilder(Set(input), None)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(dm => dm.name == "doRecursiveCalls" && dm.getEnclosingClass.get.thisType == "Calls").get)

      val result = builder.resolveFrom(sourceDm)

      // It should be possible to build this graph, and the only instantiatable types afterwards should be the three involved types
      assert(result.isSuccess)
      val instantiatedTypes = result.get
      assert(instantiatedTypes.nonEmpty)
      assert(instantiatedTypes.contains("BranchingTaint") &&
        instantiatedTypes.contains("Calls") &&
        instantiatedTypes.contains("CallTargetImpl"))
      assert(instantiatedTypes.size == 3)

      assert(builder.calleesOf.contains(sourceDm))
      val res1 = builder.calleesOf(sourceDm)
      assert(res1.contains(4) && res1.contains(10))
      assert(res1(10).size == 2)
      assert(res1(10).exists(_.definingTypeName == "Calls"))
      assert(res1(10).exists(_.definingTypeName == "CallTargetImpl"))

      val recurseSuper = builder.calleesOf.keys.find(dm => dm.methodName == "recurse" && dm.definingTypeName == "Calls").get
      assert(builder.calleesOf.contains(recurseSuper))
      val res2 = builder.calleesOf(recurseSuper)
      assert(res2.size == 3)
      assert(res2.contains(1) && res2.contains(8) && res2.contains(13))
      assert(res2(1).size == 2)
      assert(res2(13).size == 2)
    }

  }

}
