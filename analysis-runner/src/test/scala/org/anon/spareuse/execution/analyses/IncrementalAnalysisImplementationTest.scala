package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.formats.{EmptyFormat, ListResultFormat}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod}
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.testutils.{AnalysisTestDataFactory, SoftwareEntityTestDataFactory}
import org.scalatest.funspec.AnyFunSpec

import scala.util.{Success, Try}

class IncrementalAnalysisImplementationTest extends AnyFunSpec {

  type IncrementalAnalysisStep = (Seq[SoftwareEntityData], Option[AnalysisResultData]) => Try[Set[AnalysisResult]]

  describe("Any incremental analysis implementation"){

    it("should not execute the analysis for no inputs"){
      val dummyImpl = buildDummyImpl(buildDummyDescriptor(SoftwareEntityKind.Method, batch = true), None){ (inputs, prevResult) =>
        fail("Analysis step should not be executed for no inputs")
      }

      dummyImpl.executeAnalysis(Seq.empty, "")
    }

    it("should execute the analysis once for all inputs if no baseline is specified"){

      var executionStepCnt = 0
      val dummyInputs: Seq[SoftwareEntityData] = for( i <- Range(0, 3)) yield SoftwareEntityTestDataFactory.genericEntity(name = s"group:artifact:$i")

      val dummyImpl = buildDummyImpl(buildDummyDescriptor(SoftwareEntityKind.Method, batch = true), None){ (inputs, prevResult) =>
        executionStepCnt += 1
        assert(prevResult.isEmpty)
        assert(inputs.size == dummyInputs.size)
        Success(Set.empty)
      }

      val result = dummyImpl.executeAnalysis(dummyInputs, "")

      assert(executionStepCnt == 1)
      assert(result.isSuccess && result.get.isEmpty)
    }

    it("should execute the analysis three times if baselines exist for all three inputs"){
      val dummyAnalysisDescriptor = buildDummyDescriptor(SoftwareEntityKind.Method, batch = true)

      val baselineM1 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:c:1.0.0", "my.package", "MyClass", "doFoo")
      val baselineM2 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doBar")
      val baselineM3 = SoftwareEntityTestDataFactory.methodFor(baselineM1.getParent.get.asInstanceOf[JavaClass], "doNothing", returnType = "Void")
      val baselineEntities = Set(baselineM1, baselineM2, baselineM3)

      val currentM1 = SoftwareEntityTestDataFactory.fullMethodEntity("a.b:c:3.0.0", "my.package", "MyClass", "doFoo")
      val currentM2 = SoftwareEntityTestDataFactory.methodFor(currentM1.getParent.get.asInstanceOf[JavaClass], "doBar")
      val currentM3 = SoftwareEntityTestDataFactory.methodFor(currentM1.getParent.get.asInstanceOf[JavaClass], "doNothing", returnType = "Void")
      val currentEntities = Set(currentM1, currentM2, currentM3)

      val baselineResults = baselineEntities.map{ e => AnalysisTestDataFactory.stringResult(e.name, Set(e), uid = s"DUMMY-RESULT-${e.name}")}
      val baselineRun = AnalysisTestDataFactory.analysisRun(dummyAnalysisDescriptor.name, dummyAnalysisDescriptor.version, baselineResults, baselineEntities.map(_.asInstanceOf[SoftwareEntityData]))

      var executionStepCnt = 0

      val dummyImpl = buildDummyImpl(dummyAnalysisDescriptor, Some(baselineRun)){ (inputs, prevResult) =>

        assert(inputs.size == 1)
        assert(inputs.head.isMethod)
        assert(prevResult.isDefined)

        val resContent = prevResult.get.content.asInstanceOf[String]
        val currentInputName = inputs.head.asInstanceOf[JavaMethod].name

        assert(resContent.equals(currentInputName))

        executionStepCnt += 1
        Success(Set.empty)
      }

      val result = dummyImpl.executeAnalysis(currentEntities.toSeq.map(_.asInstanceOf[SoftwareEntityData]), "")

      assert(executionStepCnt == 3)

    }

  }

  private def buildDummyDescriptor(inputKind: SoftwareEntityKind, batch: Boolean): AnalysisImplementationDescriptor = new AnalysisImplementationDescriptor {
    override val analysisData: AnalysisData = new AnalysisData("demo-incremental-analysis", "1.0.0", "",
      "", "system", Set.empty, false, ListResultFormat(EmptyFormat), inputKind, batch, true, Set.empty)
  }

  private def buildDummyImpl(d: AnalysisImplementationDescriptor,
                             baseline: Option[AnalysisRunData])
                            (implicit impl: IncrementalAnalysisStep): IncrementalAnalysisImplementation = new IncrementalAnalysisImplementation(baseline) {

    override def executeIncremental(inputs: Seq[SoftwareEntityData],
                                    previousResult: Option[AnalysisResultData]): Try[Set[AnalysisResult]] = impl(inputs, previousResult)

    override val descriptor: AnalysisImplementationDescriptor = d

    override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = true
  }


}
