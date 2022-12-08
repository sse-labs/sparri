package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.formats
import de.tudo.sse.spareuse.core.formats.json.CustomObjectWriter
import de.tudo.sse.spareuse.core.formats.{MapResultFormat, NamedPropertyFormat, NumberFormat, ObjectResultFormat}
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{buildClassFor, buildLibrary, buildPackageFor, buildProgramFor}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must


class MvnConstantClassAnalysisImplTest extends AnyFlatSpec with must.Matchers {

  private val analysis = new MvnConstantClassAnalysisImpl
  private val sampleLibrary = buildLibrary("org.springframework:spring-jmx")
  private val sampleProgram = buildProgramFor(sampleLibrary, "org.springframework:spring-jmx:2.0.5")

  private val expectedResultFormat = MapResultFormat(formats.StringFormat,
    ObjectResultFormat(Set(NamedPropertyFormat("noOfOccurrences", NumberFormat), NamedPropertyFormat("noOfUniqueOccurrences", NumberFormat))))


  "The constant class analysis" must "reject invalid inputs" in {


    assert(!analysis.executionPossible(Seq(sampleProgram), ""))

    assert(analysis.executionPossible(Seq(sampleLibrary), "abfc"))
  }

  "The constant class analysis" must "fail for corrupt inputs" in {
    val lib = buildLibrary("abc:def")
    val prog = buildProgramFor(lib, "abc:def:1.0")

    val samplePackage = buildPackageFor(prog, "test1")
    val samplePackage2 = buildPackageFor(prog, "test2")
    // Create corrupt hierarchy where there is a package on the class level
    samplePackage2.setParent(samplePackage)

    assert(analysis.executionPossible(Seq(lib), ""))

    val failedResult = analysis.executeAnalysis(Seq(lib), "")
    assert(failedResult.isFailure && failedResult.failed.get.isInstanceOf[IllegalArgumentException])

  }

  "The constant class analysis" must "calculate valid results" in {
    val lib = buildLibrary("abc:def")

    val prog1 = buildProgramFor(lib, "abc:def:1.0")
    val prog2 = buildProgramFor(lib, "abc:def:1.1")
    val prog3 = buildProgramFor(lib, "abc:def:1.2")

    val samplePackage1 = buildPackageFor(prog1, "test1")
    val samplePackage2 = buildPackageFor(prog2, "test1")
    val samplePackage3 = buildPackageFor(prog3, "test1")

    // Same class in three releases. Two different hashes expected
    buildClassFor(samplePackage1, "test", "test1/test", hash = Array(3.byteValue(), 4.byteValue()))
    buildClassFor(samplePackage2, "test", "test1/test", hash = Array(3.byteValue(), 4.byteValue()))
    buildClassFor(samplePackage3, "test", "test1/test", hash = Array(2.byteValue(), 4.byteValue()))


    assert(analysis.executionPossible(Seq(lib), ""))

    val result = analysis.executeAnalysis(Seq(lib), "")

    assert(result.isSuccess && result.get.size == 1)

    val data = result.get.head

    assert(data.affectedEntities.contains(lib) && data.affectedEntities.size == 1)

    val writer = new CustomObjectWriter(expectedResultFormat)
    val json = writer.write(data.content)

    println(json.prettyPrint)

    assert(expectedResultFormat.isValid(json))



    val resultData = json.asJsObject.fields

    assert(resultData.contains("test1/test"))

    val classResult = resultData("test1/test").asJsObject.fields

    assert(classResult.contains("noOfOccurrences"))
    assert(classResult.contains("noOfUniqueOccurrences"))


  }

}
