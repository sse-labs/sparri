package de.tudo.sse.spareuse.execution.commands

import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, StartRunCommand}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must

class RunnerCommandTest extends AnyFlatSpec with must.Matchers {

  "The runner command serialization" must "perform a valid roundtrip serialization" in {
    val startCmd = StartRunCommand("dep-analysis", "my-user", Set("test", "entity"), "some-config=")

    val json = RunnerCommand.toJson(startCmd)

    println(json)

    val cmd = RunnerCommand.fromJson(json)

    assert(cmd.isSuccess)
    assert(cmd.get.equals(startCmd))
  }

}
