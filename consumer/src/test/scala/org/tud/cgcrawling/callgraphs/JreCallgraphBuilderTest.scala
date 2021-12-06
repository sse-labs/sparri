package org.tud.cgcrawling.callgraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must

import scala.util.{Failure, Success}

class JreCallgraphBuilderTest extends AnyFlatSpec with must.Matchers {

  "The JRE CG builder" must "build the CG for the JRE" in {
    JreCallgraphBuilder.buildCallgraphEvolution() match {
      case Success(evo) =>
        println("Methods: " + evo.numberOfMethodEvolutions())
        println("Invocations: " + evo.numberOfInvocationEvolutions())
        println("Instantiated Types: " + evo.numberOfInstantiatedTypeEvolutions())
      case Failure(ex) => fail(ex)
    }
  }

}
