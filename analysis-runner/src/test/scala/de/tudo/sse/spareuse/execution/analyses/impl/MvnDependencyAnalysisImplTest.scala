package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.formats
import de.tudo.sse.spareuse.core.formats.json.CustomObjectWriter
import de.tudo.sse.spareuse.core.formats.{ListResultFormat, MapResultFormat, NamedPropertyFormat, ObjectResultFormat}
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaProgram, buildPackage, buildPackageFor}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must

class MvnDependencyAnalysisImplTest extends AnyFlatSpec with must.Matchers {

  private val emptyConfig = ""
  private val sampleProgram = new JavaProgram("org.springframework:spring-jmx:2.0.5",
    "org.springframework:spring-jmx!org.springframework:spring-jmx:2.0.5", "org.springframework:spring-jmx!org.springframework:spring-jmx:2.0.5", "mvn", Array.empty)
  private val samplePackage = buildPackageFor(sampleProgram, "test")

  private val expectedResultFormat = ListResultFormat(ObjectResultFormat(Set(NamedPropertyFormat("identifier", ObjectResultFormat(Set(NamedPropertyFormat("groupId", formats.StringFormat), NamedPropertyFormat("artifactId", formats.StringFormat), NamedPropertyFormat("version", formats.StringFormat)))), NamedPropertyFormat("scope", formats.StringFormat))))



  "The dependency analysis implementation" must "produce valid results in the correct format" in {
    val analysis = new MvnDependencyAnalysisImpl

    assert(analysis.executionPossible(Seq(sampleProgram), emptyConfig))

    val result = analysis.executeAnalysis(Seq(sampleProgram), emptyConfig)

    assert(result.isSuccess)
    assert(result.get.size == 1)
    assert(result.get.head.affectedEntities.size == 1 && result.get.head.affectedEntities.head.equals(sampleProgram))

    val writer = new CustomObjectWriter(expectedResultFormat)
    val json = writer.write(result.get.head.content)

    println(json.prettyPrint)

    assert(expectedResultFormat.isValid(json))
  }

  "The dependency analysis implementation" must "must reject execution for invalid inputs" in {
    val analysis = new MvnDependencyAnalysisImpl

    assert(!analysis.executionPossible(Seq(samplePackage), emptyConfig))

    assert(!analysis.executionPossible(Seq(sampleProgram), "--invalid-config"))
  }

}
