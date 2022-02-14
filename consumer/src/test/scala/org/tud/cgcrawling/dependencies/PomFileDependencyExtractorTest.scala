package org.tud.cgcrawling.dependencies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier

class PomFileDependencyExtractorTest extends AnyFlatSpec with must.Matchers {

  val config: Configuration = new Configuration()

  val aetherExtractor = new PomFileDependencyExtractor(config)
  val jekaExtractor = new JekaDependencyExtractor {}

  val identifier1: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "love.forte.simple-robot", "api", "2.3.0")

  "The recursive dependency extraction" must "collect dependencies at different depths" in {

    val jekaResult = jekaExtractor.resolveAllDependencies(identifier1)
    val aetherResult = aetherExtractor.resolveAllDependencies(identifier1)

    assert(jekaResult._1.isSuccess)
    assert(aetherResult._1.isSuccess)

    assert(jekaResult._1.get.nonEmpty)
    assert(aetherResult._1.get.nonEmpty)

    assert(aetherResult._1.get.size >= jekaResult._1.get.size) // Assert that Aether has more deps, because it doe not resolve conflicts
  }

  "The two resolve implementations" must "not differ for direct dependencies" in {
    val jekaResult = jekaExtractor.getDeclaredDependencies(identifier1)
    val aetherResult = aetherExtractor.resolveDependencies(identifier1)

    assert(jekaResult.isSuccess)
    assert(aetherResult.isSuccess)

    assert(jekaResult.get.nonEmpty)
    assert(aetherResult.get.nonEmpty)

    jekaResult.get.foreach(d => println(d.identifier))
    println("------------")
    aetherResult.get.foreach(d => println(d.identifier))

    assert(aetherResult.get.size >= jekaResult.get.size) //TODO: For now jeka does not resolve test scope dependencies
  }

  "The two resolve implementations" must "should be somewhat equal in execution time" in {

    val start = System.currentTimeMillis()
    jekaExtractor.getDeclaredDependencies(identifier1)
    val j1 = System.currentTimeMillis()
    aetherExtractor.resolveDependencies(identifier1)
    val a1 = System.currentTimeMillis()
    val jekaResult = jekaExtractor.resolveAllDependencies(identifier1)
    val j2 = System.currentTimeMillis()
    val aetherResult = aetherExtractor.resolveAllDependencies(identifier1)
    val a2 = System.currentTimeMillis()

    assert(jekaResult._1.isSuccess)
    assert(aetherResult._1.isSuccess)

    val jekaTime = 0.5d * ((j1 - start) + (j2 - a1))
    val aetherTime = 0.5d * ((a1 - j1) + (a2 - j2))

    println(jekaTime + " ::: " + aetherTime)
  }


}
