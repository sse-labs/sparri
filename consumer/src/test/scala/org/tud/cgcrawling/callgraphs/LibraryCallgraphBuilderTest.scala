package org.tud.cgcrawling.callgraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.model.LibraryCallGraphEvolution

class LibraryCallgraphBuilderTest extends AnyFlatSpec with must.Matchers {

  private val config = new Configuration
  private val identifier1: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "love.forte.simple-robot", "api", "2.3.0")
  private val identifier2: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "com.librato.metrics", "metrics-librato", "5.1.4")
  private val identifier3: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "com.typesafe.akka", "akka-actor_2.12", "2.6.17")
  private val identifier4: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "com.google.code.gson", "gson", "2.8.9")

  private val identifier5: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "com.fasterxml.jackson.core", "jackson-databind", "2.13.0")

  "The library callgraph builder" must "process two recent releases of jackson-databind" in {
    val evolution = buildEvolutionForArtifacts(MavenIdentifier(config.mavenRepoBase.toString, "com.fasterxml.jackson.core", "jackson-databind", "2.12.5"), identifier5)

    println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
    println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
    println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))
  }

  "The library callgraph builder" must "process a recent release of gson" in {
    val evolution = buildEvolutionForArtifacts(identifier4)

    println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
    println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
    println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))
  }

  "The library callgraph builder" must "process a recent release of akka-actor" in {
    val evolution = buildEvolutionForArtifacts(identifier3)

    println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
    println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
    println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))
  }

  "The library callgraph builder" must "process a recent release of metrics-librato" in {
    val evolution = buildEvolutionForArtifacts(identifier2)

    println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
    println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
    println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))
  }

  "The library callgraph builder" must "process a recent release of simple-robot:api" in {
    val evolution = buildEvolutionForArtifacts(identifier1)

    println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
    println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
    println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))
  }

  "The library callgraph builder" must "download 3rd party dependencies for whole-program analysis" in {

    val builder = new LibraryCallgraphBuilder(identifier5.groupId, identifier5.artifactId, config)

    val evolution = new LibraryCallGraphEvolution(identifier5.groupId, identifier5.artifactId)
    builder.processIdentifier(identifier5, evolution)

    println(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")
    assert(evolution.releases().nonEmpty)

    println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
    println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
    println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))

    builder.shutdown()

  }

  private def buildEvolutionForArtifacts(idents: MavenIdentifier*): LibraryCallGraphEvolution = {
    assert(idents.nonEmpty)
    val firstIdent = idents.head

    val builder = new LibraryCallgraphBuilder(firstIdent.groupId, firstIdent.artifactId, config)

    val evolution = new LibraryCallGraphEvolution(firstIdent.groupId, firstIdent.artifactId)

    idents.foreach(i => builder.processIdentifier(i, evolution))

    println(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")
    assert(evolution.releases().nonEmpty)

    System.gc()

    evolution

  }

  "The library callgraph builder" must "process entire libraries" in {

    var builder = new LibraryCallgraphBuilder(identifier1.groupId, identifier1.artifactId, config)
    val evolutionTry = builder.buildCallgraphEvolution()

    assert(evolutionTry.isSuccess)
    val evolution = evolutionTry.get
    builder.shutdown()
    builder = null
    System.gc()

    println(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")
    assert(evolution.releases().nonEmpty)

    println("External: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
    evolution.methodEvolutions().filter(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod).foreach(a => println(a.identifier.fullSignature))


  }
}
