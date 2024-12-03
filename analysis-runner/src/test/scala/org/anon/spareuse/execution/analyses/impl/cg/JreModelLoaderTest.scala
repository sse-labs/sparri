package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.execution.AnalysisRunnerConfig
import org.scalatest.funspec.AnyFunSpec

class JreModelLoaderTest extends AnyFunSpec with CallGraphTestSupport {

  describe("The JRE model loader") {
    it("should correctly index all versions available"){

      indexJre()// Would throw exception if there is an error

      assert(JreModelLoader.jreVersionMap.size == 1)
      assert(JreModelLoader.jreVersionMap.contains("17"))

      assert(JreModelLoader.jreVersionMap("17").representation.isSuccess)
      assert(JreModelLoader.getDefaultJre.isSuccess && JreModelLoader.getDefaultJre.get.version == "17")
    }
  }

}
