package org.anon.spareuse.core.model.entities

import org.opalj.bytecode.JRELibraryFolder
import org.scalatest.funspec.AnyFunSpec

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success}

class DefaultJREModelSerializerTest extends AnyFunSpec {

  describe("The JRE model serializer") {
    it("must serializer the current default JRE without errors"){

      val outDir = Files.createDirectory(Paths.get("test-out"))

      val jreLocation = JRELibraryFolder

      println(jreLocation)

      DefaultJREModelSerializer.serializeJreModel(JRELibraryFolder, "0.0.0", outDir.toFile) match {
        case Success(outFile) =>
          assert(outFile.exists())
          assert(outFile.getName.equals(s"jre-0.0.0.json"))
        case Failure(ex) =>
          fail(ex)
      }

    }
  }

}
