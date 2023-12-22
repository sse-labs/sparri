package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.formats.{EmptyFormat, ListResultFormat}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod, JavaProgram}
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import org.anon.spareuse.core.model.entities.{JavaEntities, SoftwareEntityData}
import org.anon.spareuse.core.testutils.{AnalysisTestDataFactory, SoftwareEntityTestDataFactory}
import org.scalatest.funspec.AnyFunSpec

import scala.util.{Failure, Success, Try}

class IncrementalAnalysisImplementationTest extends AnyFunSpec {

  type IncrementalAnalysisStep = (SoftwareEntityData, Set[AnalysisResultData]) => Try[Set[AnalysisResult]]

  describe("Any incremental analysis implementation"){

    it("should not execute the analysis for no inputs"){
      val dummyImpl = buildDummyImpl(buildDummyDescriptor(SoftwareEntityKind.Method), None){ (inputs, prevResult) =>
        fail("Analysis step should not be executed for no inputs")
      }

      val result = dummyImpl.executeAnalysis(Seq.empty, "")
      assert(result.isSuccess)
    }

    it("should execute the analysis once for each input if no baseline is specified"){

      var executionStepCnt = 0
      val dummyInputs: Seq[SoftwareEntityData] = for( i <- Range(0, 3)) yield SoftwareEntityTestDataFactory.genericEntity(name = s"group:artifact:$i")

      val dummyImpl = buildDummyImpl(buildDummyDescriptor(SoftwareEntityKind.Program), None){ (input, prevResult) =>
        executionStepCnt += 1
        assert(prevResult.isEmpty)
        assert(dummyInputs.contains(input))
        Success(Set.empty)
      }

      val result = dummyImpl.executeAnalysis(dummyInputs, "")

      assert(executionStepCnt == dummyInputs.size)
      assert(result.isSuccess && result.get.isEmpty)
    }

    it("should execute the analysis two times if baselines exist for all two inputs"){
      val dummyAnalysisDescriptor = buildDummyDescriptor(SoftwareEntityKind.Program)

      val baselineM1 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:c:1.0.0", "my.package", "MyClass", "doFoo")
      val baselineM2 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doBar")
      val baselineM3 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doNothing", returnType = "Void")

      val baselineM4 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:d:1.0.0", "other.lib", "OtherClass", "otherFoo")
      val baselineM5 = SoftwareEntityTestDataFactory.methodFor(baselineM4.getParent.get.asInstanceOf[JavaClass], "otherBar")
      val baselineM6 = SoftwareEntityTestDataFactory.methodFor(baselineM4.getParent.get.asInstanceOf[JavaClass], "otherNothing", returnType = "Void")
      val baselineEntities = Set(baselineM1, baselineM2, baselineM3, baselineM4, baselineM5, baselineM6)

      val inputP1 = SoftwareEntityTestDataFactory.fullProgram("a.b:c:2.0.0")
      val inputP2 = SoftwareEntityTestDataFactory.fullProgram("a.b:d:3.0.0")

      val inputs = Set(inputP1, inputP2)

      val baselineResults = baselineEntities.map{ e => AnalysisTestDataFactory.stringResult(e.name, Set(e), uid = s"DUMMY-RESULT-${e.name}")}
      val baselineRun = AnalysisTestDataFactory.analysisRun(dummyAnalysisDescriptor.name, dummyAnalysisDescriptor.version, baselineResults, Set(baselineM1.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram], baselineM4.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram]))

      var executionStepCnt = 0
      var i1, i2 = false

      val dummyImpl = buildDummyImpl(dummyAnalysisDescriptor, Some(baselineRun)){ (input, prevResults) =>

        if(input.equals(inputP1)){
          assert(prevResults.size == 3)
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM1)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM2)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM3)))
          i1 = true
        } else if(input.equals(inputP2)){
          assert(prevResults.size == 3)
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM4)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM5)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM6)))
          i2 = true
        } else {
          fail(s"Unexpected input $input")
        }

        executionStepCnt += 1
        Success(Set.empty)
      }



      dummyImpl.executeAnalysis(inputs.toSeq.map(_.asInstanceOf[SoftwareEntityData]), "") match {
        case Failure(ex) =>
          fail(ex)
        case Success(_) =>
          assert(executionStepCnt == 2)
          assert(i1 && i2)
      }

    }

    it("should not consider entities as baselines if they have a higher version or a more general kind") {
      val dummyAnalysisDescriptor = buildDummyDescriptor(SoftwareEntityKind.Program)

      val baselineM1 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:c:1.0.0", "my.package", "MyClass", "doFoo")
      val baselineM2 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doBar")
      val baselineM3 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doNothing", returnType = "Void")

      val baselineM4 = JavaEntities.buildLibrary("ab:d")
      val baselineEntities = Set(baselineM1, baselineM2, baselineM3, baselineM4)

      val inputP1 = SoftwareEntityTestDataFactory.fullProgram("a.b:c:0.5.0")
      val inputP2 = SoftwareEntityTestDataFactory.fullProgram("a.b:d:3.0.0")

      val inputs = Set(inputP1, inputP2)

      val baselineResults = baselineEntities.map { e => AnalysisTestDataFactory.stringResult(e.name, Set(e), uid = s"DUMMY-RESULT-${e.name}") }
      val baselineRun = AnalysisTestDataFactory.analysisRun(dummyAnalysisDescriptor.name, dummyAnalysisDescriptor.version, baselineResults, Set(baselineM1.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram]))

      var executionStepCnt = 0
      var i1, i2 = false

      val dummyImpl = buildDummyImpl(dummyAnalysisDescriptor, Some(baselineRun)) { (input, prevResults) =>

        if (input.equals(inputP1)) {
          assert(prevResults.isEmpty)
          i1 = true
        } else if (input.equals(inputP2)) {
          assert(prevResults.isEmpty)
          i2 = true
        } else {
          fail(s"Unexpected input $input")
        }

        executionStepCnt += 1
        Success(Set.empty)
      }


      dummyImpl.executeAnalysis(inputs.toSeq.map(_.asInstanceOf[SoftwareEntityData]), "") match {
        case Failure(ex) =>
          fail(ex)
        case Success(_) =>
          assert(executionStepCnt == 2)
          assert(i1 && i2)
      }

    }

    it("should consider all versions as baselines if semantic versioning is not used") {
      val dummyAnalysisDescriptor = buildDummyDescriptor(SoftwareEntityKind.Program)

      val baselineM1 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:c:1a", "my.package", "MyClass", "doFoo")
      val baselineM2 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doBar")
      val baselineM3 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doNothing", returnType = "Void")

      val baselineM4 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:d:2023-12-11", "other.lib", "OtherClass", "otherFoo")
      val baselineM5 = SoftwareEntityTestDataFactory.methodFor(baselineM4.getParent.get.asInstanceOf[JavaClass], "otherBar")
      val baselineM6 = SoftwareEntityTestDataFactory.methodFor(baselineM4.getParent.get.asInstanceOf[JavaClass], "otherNothing", returnType = "Void")
      val baselineEntities = Set(baselineM1, baselineM2, baselineM3, baselineM4, baselineM5, baselineM6)

      val inputP1 = SoftwareEntityTestDataFactory.fullProgram("a.b:c:2.0")
      val inputP2 = SoftwareEntityTestDataFactory.fullProgram("a.b:d:3.0.0")

      val inputs = Set(inputP1, inputP2)

      val baselineResults = baselineEntities.map { e => AnalysisTestDataFactory.stringResult(e.name, Set(e), uid = s"DUMMY-RESULT-${e.name}") }
      val baselineRun = AnalysisTestDataFactory.analysisRun(dummyAnalysisDescriptor.name, dummyAnalysisDescriptor.version, baselineResults, Set(baselineM1.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram], baselineM4.findFirstParent(_.isProgram).get.asInstanceOf[JavaProgram]))

      var executionStepCnt = 0
      var i1, i2 = false

      val dummyImpl = buildDummyImpl(dummyAnalysisDescriptor, Some(baselineRun)) { (input, prevResults) =>

        if (input.equals(inputP1)) {
          assert(prevResults.size == 3)
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM1)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM2)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM3)))
          i1 = true
        } else if (input.equals(inputP2)) {
          assert(prevResults.size == 3)
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM4)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM5)))
          assert(prevResults.exists(r => r.affectedEntities.size == 1 && r.affectedEntities.contains(baselineM6)))
          i2 = true
        } else {
          fail(s"Unexpected input $input")
        }

        executionStepCnt += 1
        Success(Set.empty)
      }


      dummyImpl.executeAnalysis(inputs.toSeq.map(_.asInstanceOf[SoftwareEntityData]), "") match {
        case Failure(ex) =>
          fail(ex)
        case Success(_) =>
          assert(executionStepCnt == 2)
          assert(i1 && i2)
      }

    }

  }

  private def buildDummyDescriptor(inputKind: SoftwareEntityKind): AnalysisImplementationDescriptor = new AnalysisImplementationDescriptor {
    override val analysisData: AnalysisData = new AnalysisData("demo-incremental-analysis", "1.0.0", "",
      "", "system", Set.empty, false, ListResultFormat(EmptyFormat), inputKind, true, true, Set.empty)
  }

  private def buildDummyImpl(d: AnalysisImplementationDescriptor,
                             baseline: Option[AnalysisRunData])
                            (implicit impl: IncrementalAnalysisStep): IncrementalAnalysisImplementation = new IncrementalAnalysisImplementation(baseline) {

    override def executeIncremental(input: SoftwareEntityData,
                                    previousResults: Set[AnalysisResultData],
                                    rawConfig: String): Try[Set[AnalysisResult]] = impl(input, previousResults)

    override val descriptor: AnalysisImplementationDescriptor = d

    override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = true
  }


}
