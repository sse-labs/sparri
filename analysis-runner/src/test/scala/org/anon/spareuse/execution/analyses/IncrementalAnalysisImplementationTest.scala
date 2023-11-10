package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.formats.{EmptyFormat, ListResultFormat}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.testutils.SoftwareEntityTestDataFactory
import org.scalatest.funspec.AnyFunSpec

import scala.util.{Success, Try}

class IncrementalAnalysisImplementationTest extends AnyFunSpec {

  type IncrementalAnalysisStep = (Seq[SoftwareEntityData], Option[AnalysisResultData]) => Try[Set[Result]]

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

    //TODO: More elaborate tests on how analysis steps are being scheduled

  }

  private def buildDummyDescriptor(inputKind: SoftwareEntityKind, batch: Boolean): AnalysisImplementationDescriptor = new AnalysisImplementationDescriptor {
    override val analysisData: AnalysisData = new AnalysisData("demo-incremental-analysis", "1.0.0", "",
      "", "system", Set.empty, false, ListResultFormat(EmptyFormat), inputKind, batch, true, Set.empty)
  }

  private def buildDummyImpl(d: AnalysisImplementationDescriptor,
                             baseline: Option[AnalysisRunData])
                            (implicit impl: IncrementalAnalysisStep): IncrementalAnalysisImplementation = new IncrementalAnalysisImplementation(baseline) {

    override def executeIncremental(inputs: Seq[SoftwareEntityData],
                                    previousResult: Option[AnalysisResultData]): Try[Set[Result]] = impl(inputs, previousResult)

    override val descriptor: AnalysisImplementationDescriptor = d

    override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = true
  }


}
