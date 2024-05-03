package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.testutils.SoftwareEntityTestDataFactory
import org.scalatest.funspec.AnyFunSpec

class MvnTaintCandidatesFinderAnalysisImplTest extends AnyFunSpec {

  private val mockDataAccessor_NoResults = mockDataAccessor_JsonResultsOnly((_, _) => Set.empty)

  describe("The taint candidate analysis"){

    it("should correctly initialize projects with no dependencies") {
      val theAnalysis = new MvnTaintCandidatesFinderAnalysisImpl(mockDataAccessor_NoResults)
      val theInput = Seq(SoftwareEntityTestDataFactory.fullProgram("com.google.code.gson:gson:2.10"))

      assert(theAnalysis.executionPossible(theInput, ""))

      assert(theAnalysis.executeAnalysis(theInput, "").isSuccess)
    }

  }



}
