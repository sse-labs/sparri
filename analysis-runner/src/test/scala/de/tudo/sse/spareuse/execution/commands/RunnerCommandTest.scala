package de.tudo.sse.spareuse.execution.commands

import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import spray.json._

class RunnerCommandTest extends AnyFlatSpec with must.Matchers with RunnerCommandJsonSupport{

  "The runner command serialization" must "perform a valid roundtrip serialization" in {
    val startCmd = RunnerCommand("dep-analysis", "my-user", Set("test", "entity"), "some-config=")

    val json = startCmd.toJson.compactPrint

    println(json)

    val cmd = json.parseJson.convertTo[RunnerCommand]

    assert(cmd.equals(startCmd))
  }

}
