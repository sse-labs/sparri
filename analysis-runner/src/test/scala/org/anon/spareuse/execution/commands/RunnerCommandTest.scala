package org.anon.spareuse.execution.commands

import org.anon.spareuse.core.model.analysis.{AnalysisCommand, IncrementalAnalysisCommand, RunnerCommand, RunnerCommandJsonSupport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import spray.json._

class RunnerCommandTest extends AnyFlatSpec with must.Matchers with RunnerCommandJsonSupport{

  "The runner command serialization" must "perform a valid roundtrip serialization" in {
    val startCmd = AnalysisCommand("dep-analysis", "some-id", "my-user", Set("test", "entity"), "some-config=")

    val json = startCmd.toJson.compactPrint

    println(json)

    val cmd = json.parseJson.convertTo[RunnerCommand]

    assert(cmd.isInstanceOf[AnalysisCommand])
    assert(cmd.equals(startCmd))
  }

  "The runner command serialization" must "correctly deserialize an IncrementalAnalysisCommand" in {
    val startCmd = IncrementalAnalysisCommand("dep-analysis", "some-id", "my-user", Set("test", "entity"), "some-config=", "some-other-id")

    val json = startCmd.toJson.compactPrint

    println(json)

    val cmd = json.parseJson.convertTo[RunnerCommand]

    assert(cmd.isInstanceOf[IncrementalAnalysisCommand])
    assert(cmd.equals(startCmd))
  }

}
