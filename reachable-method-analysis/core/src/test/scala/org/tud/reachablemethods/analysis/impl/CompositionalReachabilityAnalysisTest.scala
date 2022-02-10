package org.tud.reachablemethods.analysis.impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.reachablemethods.analysis.Configuration
import org.tud.reachablemethods.analysis.model.{MavenIdentifier, MavenJarFileDependency}
import org.tud.reachablemethods.analysis.testutils.withActorSystem

import java.io.File

class CompositionalReachabilityAnalysisTest extends AnyFlatSpec with must.Matchers {
  "The analysis" must "always allow analyses for empty requirements" in withActorSystem { system =>

    val analysis = new CompositionalReachabilityAnalysis(new Configuration)

    assert(analysis.analysisPossible(List.empty))
  }

  "The analysis" must "always forbid analyses for non-existing requirements" in withActorSystem { system =>
    val analysis = new CompositionalReachabilityAnalysis(new Configuration)

    assert(!analysis.analysisPossible(List(MavenIdentifier("invalid---", "---invalid", "nothing"))))
  }

  "The analysis" must "correctly check actual requirements" in withActorSystem { system =>
    val analysis = new CompositionalReachabilityAnalysis(new Configuration)

    assert(analysis.analysisPossible(List(MavenIdentifier("<none>", "<jre>", "_"))))
  }

  "The analysis" must "correctly be instantiated for valid projects" in withActorSystem { system =>
    val analysis = new CompositionalReachabilityAnalysis(new Configuration)

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

    //assert(analysis.analysisPossible(deps.map(_.dependencyIdentifier)))

    val classesRoot = new File(getClass.getResource("/validproject/classes").getPath)
    val result = analysis.analyzeMavenProject(classesRoot, deps.map(_.dependencyIdentifier), treatProjectAsLibrary = false)

    assert(result.isSuccess)
  }

  "The analysis" must "produce results equivalent to regular analysis" in withActorSystem { system =>


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
    val classesRoot = new File(getClass.getResource("/validproject/classes").getPath)

    val dependencyClasses = deps.flatMap(_.classContainer.getClassList(true).get).toList
    val regularAnalysis = new RegularOpalReachabilityAnalysis(dependencyClasses)
    val compositionalAnalysis = new CompositionalReachabilityAnalysis(new Configuration)

    assert(regularAnalysis.analysisPossible(deps.map(_.dependencyIdentifier)))
    assert(compositionalAnalysis.analysisPossible(deps.map(_.dependencyIdentifier)))


    val regularResult = regularAnalysis.analyzeMavenProject(classesRoot, deps.map(_.dependencyIdentifier), treatProjectAsLibrary = false)
    val compositionalResult = compositionalAnalysis.analyzeMavenProject(classesRoot, deps.map(_.dependencyIdentifier), treatProjectAsLibrary = false)

    assert(regularResult.isSuccess)
    assert(compositionalResult.isSuccess)

    println(s"Regular analysis found ${regularResult.get.size} reachable methods")
    println(s"Compositional analysis found ${compositionalResult.get.size} reachable methods")

    val regMethods = regularResult.get
    val compMethods = compositionalResult.get

    val uniqueRegMethods = regMethods.count(regM => !compMethods.contains(regM))
    val uniqueCompMethods = compMethods.count(compM => !regMethods.contains(compM))

    println(s"Regular result has $uniqueRegMethods unique methods, compositional result has $uniqueCompMethods unique methods.")
  }
}
