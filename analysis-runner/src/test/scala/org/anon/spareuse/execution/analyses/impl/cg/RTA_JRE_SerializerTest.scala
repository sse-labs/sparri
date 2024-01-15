package org.anon.spareuse.execution.analyses.impl.cg

import org.opalj.bytecode.JRELibraryFolder
import org.scalatest.funspec.AnyFunSpec

import java.io.File
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success}

class RTA_JRE_SerializerTest extends AnyFunSpec {

  describe("The dedicated RTA JRE serializer"){
    ignore("must serialize the current JRE without error"){
      val outDir = Files.createDirectory(Paths.get("test-out"))

      val jreLocation = new File("C:\\Program Files\\Java\\jre-1.8\\lib")
      //val jreLocation = JRELibraryFolder

      println(jreLocation)

      RTA_JRE_Serializer.serializeJreModel(jreLocation, "8", outDir.toFile) match {
        case Success(outFile) =>
          assert(outFile.exists())
          assert(outFile.getName.equals(s"jre-8.json"))
        case Failure(ex) =>
          fail(ex)
      }
    }
  }

}
