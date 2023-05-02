package de.tudo.sse.spareuse.playground

import de.tudo.sse.spareuse.eval.performance.dependencies.SimpleTransitiveDependencyAnalysis

import scala.util.{Failure, Success}

object DependencyTester extends App {

  val gav = "nl.basjes.parse.useragent:yauaa:7.0.0"

  val analysis = new SimpleTransitiveDependencyAnalysis

  analysis.getAllDependencies(gav) match {
    case Success(deps) =>
      deps.foreach(println)
    case Failure(ex) =>
      println("Error while loading dependencies: " + ex.getMessage)
      ex.printStackTrace()

  }



}
