package org.tud.reachablemethods.analysis.impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.reachablemethods.analysis.Configuration
import org.tud.reachablemethods.analysis.model.MavenIdentifier
import org.tud.reachablemethods.analysis.testutils.withActorSystem

class CompositionalReachabilityAnalysisTest extends AnyFlatSpec with must.Matchers {
  "The analysis" must "always allow analyses for empty requirements" in withActorSystem { system =>

    val analysis = new CompositionalReachabilityAnalysis(new Configuration)(system)

    assert(analysis.analysisPossible(List.empty))
  }

  "The analysis" must "always forbid analyses for non-existing requirements" in withActorSystem { system =>
    val analysis = new CompositionalReachabilityAnalysis(new Configuration)(system)

    assert(!analysis.analysisPossible(List(MavenIdentifier("invalid---", "---invalid", "nothing"))))
  }

  "The analysis" must "correctly check actual requirements" in withActorSystem { system =>
    val analysis = new CompositionalReachabilityAnalysis(new Configuration)(system)

    assert(analysis.analysisPossible(List(MavenIdentifier("<none>", "<jre>", "_"))))
  }
}
