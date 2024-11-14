package org.anon.spareuse.client

import org.scalatest.funspec.AnyFunSpec

import java.io.File
import java.nio.file.Paths

class ClientAnalysisApplicationITest extends AnyFunSpec {
  describe("The client application"){

    it("should work on actual Maven projects"){
      val projectRoot = new File(getClass.getClassLoader.getResource("modular-analysis-demo-target").toURI)
      assert(projectRoot.exists())

      val cmdArgs = Array(Paths.get(projectRoot.getPath, "target", "classes").toAbsolutePath.toString, Paths.get(projectRoot.getPath, "pom.xml").toAbsolutePath.toString)

      ClientAnalysisApplication.main(cmdArgs)
    }

  }
}
