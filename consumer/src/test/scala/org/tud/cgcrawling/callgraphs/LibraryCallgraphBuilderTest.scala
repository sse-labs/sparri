package org.tud.cgcrawling.callgraphs

import akka.actor.ActorSystem
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.model.LibraryCallGraphEvolution

import scala.util.{Failure, Try}

class LibraryCallgraphBuilderTest extends AnyFlatSpec with must.Matchers {

  private val config = new Configuration
  private val identifier1: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "love.forte.simple-robot", "api", "2.3.0")
  private val identifier2: MavenIdentifier =
    MavenIdentifier(config.mavenRepoBase.toString, "com.librato.metrics", "metrics-librato", "5.1.4")

  "The library callgraph builder" must "download 3rd party dependencies for whole-program analysis" in {
    val system = ActorSystem("test-lib-cg-builder")

    Try{
      val builder = new LibraryCallgraphBuilder(identifier1.groupId, identifier1.artifactId, config)(system)

      val evolution = new LibraryCallGraphEvolution(identifier1.groupId, identifier1.artifactId)
      builder.processIdentifier(identifier1, evolution)

      println(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")
      assert(evolution.releases().nonEmpty)

      println("Project: " + evolution.methodEvolutions().count(mEvo => !mEvo.identifier.isExternal))
      println("JRE: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && mEvo.identifier.isJREMethod))
      println("3rd party: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
      println("With defining Artifact: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.definingArtifact.isDefined))

      builder.shutdown()
    } match {
      case Failure(ex) =>
        system.terminate()
        throw ex
      case _ =>
    }
  }

  "The library callgraph builder" must "process entire libraries" in {
    val system = ActorSystem("test-lib-cg-builder")

    Try{
      val builder = new LibraryCallgraphBuilder(identifier1.groupId, identifier1.artifactId, config)(system)
      val evolutionTry = builder.buildCallgraphEvolution()

      assert(evolutionTry.isSuccess)
      val evolution = evolutionTry.get

      println(s"Got a total of ${evolution.numberOfDependencyEvolutions()} dependencies, ${evolution.releases().size} releases with ${evolution.numberOfMethodEvolutions()} methods and ${evolution.numberOfInvocationEvolutions()} invocations")
      assert(evolution.releases().nonEmpty)

      println("External: " + evolution.methodEvolutions().count(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod))
      evolution.methodEvolutions().filter(mEvo => mEvo.identifier.isExternal && !mEvo.identifier.isJREMethod).foreach(a => println(a.identifier.fullSignature))


      builder.shutdown()
    } match {
      case Failure(ex) =>
        system.terminate()
        throw ex
      case _ =>
    }
  }
}
