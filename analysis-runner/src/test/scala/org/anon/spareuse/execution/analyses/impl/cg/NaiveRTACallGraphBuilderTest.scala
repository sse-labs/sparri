package org.anon.spareuse.execution.analyses.impl.cg

import org.opalj.br.MethodDescriptor
import org.scalatest.funspec.AnyFunSpec

class NaiveRTACallGraphBuilderTest extends AnyFunSpec with CallGraphTestSupport {

  describe("The naive RTA CG Builder") {
    it("should correctly stitch the type hierarchy without the JRE present"){
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), None)

      cgOpalProject.allClassFiles.map(_.fqn).foreach { fqn =>
        assert(builder.typeLookup.contains(fqn))
      }

      val callTargetImplNode = builder.typeLookup("CallTargetImpl")
      assert(callTargetImplNode.hasParent)
      assert(callTargetImplNode.hasInterfaces)
      assert(callTargetImplNode.getInterfaces.size == 1)

      val theParent = callTargetImplNode.getParent.get
      assert(theParent.thisType == "Calls")
      assert(!theParent.hasParent)
      assert(theParent.superTypeOpt.isDefined && theParent.superTypeOpt.get == objFqn)
      assert(theParent.isIncomplete)
      assert(!theParent.hasInterfaces)

      val theInterface = callTargetImplNode.getInterfaces.head
      assert(theInterface.isInterface)
      assert(!theInterface.hasParent)
      assert(!theInterface.hasInterfaces)
      assert(theInterface.thisType == "CallTarget")

      assert(theInterface.getChildren.contains(callTargetImplNode))
    }

    it("should build a type hierarchy for the JRE itself"){
      val builder = new NaiveRTACallGraphBuilder(Set(jreObjectModel), None)

      assert(builder.typeLookup.nonEmpty)

      val objNode = builder.typeLookup(objFqn)
      assert(!objNode.hasParent)
      assert(!objNode.hasInterfaces)
      assert(!objNode.isIncomplete)

      println(s"Object type has ${objNode.getChildren.size} direct children")
    }

    it("should correctly stitch the type hierarchy with the JRE present"){
      resetModelLoader()
      val input = getCgFixtureModel

      val builder = new NaiveRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))

      cgOpalProject.allClassFiles.map(_.fqn).foreach { fqn =>
        assert(builder.typeLookup.contains(fqn))
      }

      val callsNode = builder.typeLookup("Calls")
      assert(callsNode.hasParent)
      assert(callsNode.getParent.get.thisType == objFqn)
      assert(!callsNode.isIncomplete)
    }

    it("should work for static calls in naive mode even on incomplete type hierarchies"){
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), None)

      val result = builder.buildNaive()
      assert(result.isSuccess)

      val doStaticDMOpt = builder.calleesOf.keys.find(dm => dm.methodName == "doStaticCalls" && dm.definingTypeName == "Calls")
      assert(doStaticDMOpt.isDefined)
      val doStaticDM = doStaticDMOpt.get

      assert(builder.calleesOf(doStaticDM).size == 2)
      assert(builder.calleesOf(doStaticDM).contains(0) && builder.calleesOf(doStaticDM).contains(5))

      val res1 = builder.calleesOf(doStaticDM)(0)
      val res2 = builder.calleesOf(doStaticDM)(5)

      assert(res1.size == 1)
      assert(res2.size == 1)

      val r1 = res1.head
      val r2 = res2.head

      assert(r1 == r2)
      assert(r1.definingTypeName == "Calls")
      assert(r1.methodName == "doFoo")
      assert(MethodDescriptor(r1.descriptor) == MethodDescriptor.NoArgsAndReturnVoid)
    }

    it("should resolve some virtual calls in naive mode even on incomplete type hierarchies"){
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), None)

      val result = builder.buildNaive()
      assert(result.isSuccess)


      val doVirtDMOpt = builder.calleesOf.keys.find(dm => dm.methodName == "doVirtualCalls" && dm.definingTypeName == "Calls")
      assert(doVirtDMOpt.isDefined)
      val doVirtDM = doVirtDMOpt.get

      // Expect four INVOKESPECIAL and three INVOKEVIRTUAL (Object.toString cannot be resolved here!)
      assert(builder.calleesOf(doVirtDM).size == 7)
      assert(!builder.calleesOf(doVirtDM).contains(15)) // Make sure "toString" on "Object" is resolved with JRE
      val expectedCallSitePcs = Set(28, 40, 54)
      assert(expectedCallSitePcs.forall(pc => builder.calleesOf(doVirtDM).contains(pc)))




      val callSite1Targets = builder.calleesOf(doVirtDM)(28)
      assert(callSite1Targets.size == 2)
      assert(callSite1Targets.exists(dm => dm.definingTypeName == "Calls" && dm.methodName == "doStaticCalls" &&
        MethodDescriptor(dm.descriptor) == MethodDescriptor.NoArgsAndReturnVoid))
      assert(callSite1Targets.exists(dm => dm.definingTypeName == "CallTargetImpl" && dm.methodName == "doStaticCalls" &&
        MethodDescriptor(dm.descriptor) == MethodDescriptor.NoArgsAndReturnVoid))

      val callSite2Targets = builder.calleesOf(doVirtDM)(40)
      assert(callSite2Targets.size == 2)
      assert(callSite2Targets.exists(dm => dm.definingTypeName == "Calls" && dm.methodName == "doStaticCalls" &&
        MethodDescriptor(dm.descriptor) == MethodDescriptor.NoArgsAndReturnVoid))
      assert(callSite2Targets.exists(dm => dm.definingTypeName == "CallTargetImpl" && dm.methodName == "doStaticCalls" &&
        MethodDescriptor(dm.descriptor) == MethodDescriptor.NoArgsAndReturnVoid))

      val callSite3Targets = builder.calleesOf(doVirtDM)(54)
      assert(callSite3Targets.size == 1)
      val callSite3 = callSite3Targets.head
      assert(callSite3.definingTypeName == "CallTargetImpl" && callSite3.methodName == "doStaticCalls" &&
        MethodDescriptor(callSite3.descriptor) == MethodDescriptor.NoArgsAndReturnVoid)

    }

    it("should resolve all virtual calls when full JRE summaries are used in naive mode") {
      resetModelLoader()
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))

      val result = builder.buildNaive()
      assert(result.isSuccess)

      val doVirtDMOpt = builder.calleesOf.keys.find(dm => dm.methodName == "doVirtualCalls" && dm.definingTypeName == "Calls")
      assert(doVirtDMOpt.isDefined)
      val doVirtDM = doVirtDMOpt.get

      // Expect four INVOKESPECIAL and four INVOKEVIRTUAL - Object.toString must be resolved here!
      assert(builder.calleesOf(doVirtDM).size == 8)
      assert(builder.calleesOf(doVirtDM).contains(15))// Make sure "toString" on "Object" is resolved with JRE

      val toStringTargets = builder.calleesOf(doVirtDM)(15)

      assert(toStringTargets.nonEmpty)
      val allInstantiatedTypes = JreModelLoader.getDefaultJre.get.allTypesInstantiated ++ input.allClasses.flatMap(c => c.getMethods.flatMap(m => m.newStatements.map(_.instantiatedTypeName)))
      assert(toStringTargets.size <= allInstantiatedTypes.size)

      // Check that (of the fixture classes) only the one defining a toString method is invoked
      assert(toStringTargets.forall(dm => dm.methodName == "toString" && MethodDescriptor(dm.descriptor) == MethodDescriptor.JustReturnsString))
      assert(toStringTargets.exists(dm => dm.definingTypeName == "CallTargetImpl"))
      assert(!toStringTargets.exists(dm => dm.definingTypeName == "Calls"))
      assert(!toStringTargets.exists(dm => dm.definingTypeName == "BranchingTaint"))
      assert(!toStringTargets.exists(dm => dm.definingTypeName == "CallTarget"))

      // Assert that all potential targets are instantiated somewhere in the code
      assert(toStringTargets.forall(dm => allInstantiatedTypes.contains(dm.definingTypeName)))

      val allCallSitesCount = builder.calleesOf.values.map(callSites => callSites.size).sum
      val allTargetsCount = builder.calleesOf.values.map(callSites => callSites.values.map(_.size).sum).sum

      println(s"Found ${builder.reachableMethods().size} reachable methods.")
      println(s"Resolved $allCallSitesCount callsites to $allTargetsCount methods")
    }

    it("should resolve recursive calls when starting at an entry point without the JRE"){
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), None)

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(dm => dm.name == "doRecursiveCalls" && dm.enclosingClass.get.thisType == "Calls").get)

      val result = builder.buildNaiveFrom(sourceDm)

      assert(result.isSuccess)

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

    it("should resolve all virtual calls when starting at an entry point with the JRE") {
      resetModelLoader()
      val input = getCgFixtureModel
      val builder = new NaiveRTACallGraphBuilder(Set(input), Some(JreModelLoader.defaultJre))

      val sourceDm = builder.asDefinedMethod(input.allMethods.find(_.name == "doVirtualCalls").get)

      val result = builder.buildNaiveFrom(sourceDm)
      assert(result.isSuccess)

      // There should now be a resolution for Object.toString at PC 15
      assert(builder.calleesOf.contains(sourceDm) && builder.calleesOf(sourceDm).size == 8)
      assert(builder.calleesOf(sourceDm).contains(15))
      val toStringTargets = builder.calleesOf(sourceDm)(15)

      assert(toStringTargets.nonEmpty)
      val allInstantiatedTypes = JreModelLoader.getDefaultJre.get.allTypesInstantiated ++ input.allClasses.flatMap(c => c.getMethods.flatMap(m => m.newStatements.map(_.instantiatedTypeName)))
      assert(toStringTargets.size <= allInstantiatedTypes.size)

      val allCallSitesCount = builder.calleesOf.values.map(callSites => callSites.size).sum
      val allTargetsCount = builder.calleesOf.values.map(callSites => callSites.values.map(_.size).sum).sum

      println(s"Found ${builder.reachableMethods().size} reachable methods.")
      println(s"Resolved $allCallSitesCount callsites to $allTargetsCount methods")
    }

  }

}
