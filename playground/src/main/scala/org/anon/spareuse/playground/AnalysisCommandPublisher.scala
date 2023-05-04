package org.anon.spareuse.playground

import com.typesafe.config.ConfigFactory
import org.anon.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import org.anon.spareuse.core.utils.rabbitmq.{MqConfigurationBuilder, MqDirectQueuePublishConfiguration, MqMessageWriter}
import spray.json.enrichAny
import org.slf4j.{Logger, LoggerFactory}

object AnalysisCommandPublisher extends RunnerCommandJsonSupport{

  val log: Logger = LoggerFactory.getLogger(getClass)

  val writer = new MqMessageWriter(MqConfigurationBuilder.analysisWriteConfig("mvn-analysis-runner-playground"))


  def main(args: Array[String]): Unit = {

    /*writer.initialize()

    val startCmd = RunnerCommand("mvn-constant-classes:1.0.0", "test", Set("org.sonatype.tycho:maven-osgi-compiler-plugin", "org.sonatype.tycho:maven-osgi-test-plugin"), "").toJson.compactPrint
    val startCmd2 = RunnerCommand("mvn-dependencies:1.0.0", "test", Set("org.springframework:spring-jms!org.springframework:spring-jms:2.5.6.SEC03", "org.springframework:spring-jms!org.springframework:spring-jms:2.5.6"), "-no-transitive -use-jeka").toJson.compactPrint
    val startCmd3 = RunnerCommand("mvn-constant-classes:1.0.0", "test", Set("com.google.code.gson:gson"), "").toJson.compactPrint
    val startCmd4 = RunnerCommand("mvn-partial-callgraphs:1.0.0", "test", Set("com.google.code.gson:gson!com.google.code.gson:gson:2.9.0"), "").toJson.compactPrint

    log.info(s"Publishing Cmd JSON: $startCmd4")

    writer.appendToQueue(startCmd4)

    log.info("Done Publishing cmd JSON.")*/

    writer.shutdown()
  }

}


