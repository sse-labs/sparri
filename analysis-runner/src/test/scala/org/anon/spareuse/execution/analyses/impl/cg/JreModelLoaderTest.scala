package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.execution.AnalysisRunnerConfig
import org.scalatest.funspec.AnyFunSpec

class JreModelLoaderTest extends AnyFunSpec {

  describe("The JRE model loader") {
    it("should correctly index all versions available"){
      val config = new AnalysisRunnerConfig("", 10, true, "jre-data")

      JreModelLoader.indexJreData(config) // Would throw exception if there is an error

      assert(JreModelLoader.jreVersionMap.size == 1)
      assert(JreModelLoader.jreVersionMap.contains("17"))

      assert(JreModelLoader.jreVersionMap("17").representation.isSuccess)
      assert(JreModelLoader.getDefaultJre.isSuccess && JreModelLoader.getDefaultJre.get.version == "17")
    }
  }

}
