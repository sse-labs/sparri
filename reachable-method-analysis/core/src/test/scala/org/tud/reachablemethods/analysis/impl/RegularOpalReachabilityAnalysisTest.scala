package org.tud.reachablemethods.analysis.impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.reachablemethods.analysis.model.{MavenIdentifier, MavenJarFileDependency}

import java.io.File

class RegularOpalReachabilityAnalysisTest extends AnyFlatSpec with must.Matchers {

  "The analysis" must "correctly be instantiated for valid projects" in  {
    val analysis = new RegularOpalReachabilityAnalysis()

    val jaFile = new File(getClass.getResource("/validproject/deps/jackson-annotations-2.9.0.jar").getPath)
    val jcFile = new File(getClass.getResource("/validproject/deps/jackson-core-2.9.3.jar").getPath)
    val jdFile = new File(getClass.getResource("/validproject/deps/jackson-databind-2.9.3.jar").getPath)
    val jdxFile = new File(getClass.getResource("/validproject/deps/jackson-dataformat-xml-2.9.3.jar").getPath)
    val jmjaFile = new File(getClass.getResource("/validproject/deps/jackson-module-jaxb-annotations-2.9.3.jar").getPath)

    val deps = List(
      new MavenJarFileDependency(MavenIdentifier("com.fasterxml.jackson.dataformat", "jackson-dataformat-xml", "2.9.3"), jdxFile, None),
      new MavenJarFileDependency(MavenIdentifier("com.fasterxml.jackson.core", "jackson-databind", "2.9.3"), jdFile, None),
      new MavenJarFileDependency(MavenIdentifier("com.fasterxml.jackson.core", "jackson-core", "2.9.3"), jcFile, None),
      new MavenJarFileDependency(MavenIdentifier("com.fasterxml.jackson.core", "jackson-annotations", "2.9.0"), jaFile, None),
      new MavenJarFileDependency(MavenIdentifier("com.fasterxml.jackson.module", "jackson-module-jaxb-annotations", "2.9.3"), jmjaFile, None)
    )

    assert(analysis.analysisPossible(deps.map(_.dependencyIdentifier)))

    val classesRoot = new File(getClass.getResource("/validproject/classes").getPath)
    val result = analysis.analyzeMavenProject(classesRoot, deps, treatProjectAsLibrary = false)

    assert(result.isSuccess)
  }

}
