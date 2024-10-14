package org.anon.spareuse.execution.commands

import org.anon.spareuse.core.model.analysis.{AnalysisCommand, IncrementalAnalysisCommand, RunnerCommand, RunnerCommandJsonSupport}
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.storage.postgresql.PostgresDataAccessor
import org.anon.spareuse.core.utils.rabbitmq.{MqConfigurationBuilder, MqMessageWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import spray.json._

class RunnerCommandTest extends AnyFlatSpec with must.Matchers with RunnerCommandJsonSupport{

  "The runner command serialization" must "perform a valid roundtrip serialization" in {
    val startCmd = AnalysisCommand("dep-analysis", "some-id", "my-user", Set(42, 3), "some-config=")

    val json = startCmd.toJson.compactPrint

    println(json)

    val cmd = json.parseJson.convertTo[RunnerCommand]

    assert(cmd.isInstanceOf[AnalysisCommand])
    assert(cmd.equals(startCmd))
  }

  "The runner command serialization" must "correctly deserialize an IncrementalAnalysisCommand" in {
    val startCmd = IncrementalAnalysisCommand("dep-analysis", "some-id", "my-user", Set(42, 3), "some-config=", "some-other-id")

    val json = startCmd.toJson.compactPrint

    println(json)

    val cmd = json.parseJson.convertTo[RunnerCommand]

    assert(cmd.isInstanceOf[IncrementalAnalysisCommand])
    assert(cmd.equals(startCmd))
  }

  "Runner commands" must "be published" ignore {
    val analysisName = "name"
    val analysisVersion = "version"
    val config = ""
    val inputs = Set(-1L)
    val dataAccessor: DataAccessor = new PostgresDataAccessor()(scala.concurrent.ExecutionContext.global)

    // Create new empty record for analysis run
    val newId = dataAccessor.storeEmptyAnalysisRun(analysisName, analysisVersion, config).get

    // Queue run execution
    val name = s"$analysisName:$analysisVersion"
    val command = AnalysisCommand(name, newId, "Test-Runner", inputs, config)
    val commandJson = command.toJson.compactPrint


    val writer = new MqMessageWriter(MqConfigurationBuilder.analysisWriteConfig("Test-Runner-Conn"))
    writer.initialize()
    writer.appendToQueue(commandJson)
    writer.shutdown()
  }

}
