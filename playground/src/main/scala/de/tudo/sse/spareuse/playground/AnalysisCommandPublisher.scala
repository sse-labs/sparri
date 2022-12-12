package de.tudo.sse.spareuse.playground

import com.typesafe.config.ConfigFactory
import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, RunnerCommandJsonSupport}
import de.tudo.sse.spareuse.core.utils.rabbitmq.{MqDirectQueuePublishConfiguration, MqMessageWriter}
import spray.json.enrichAny
import org.slf4j.{Logger, LoggerFactory}

object AnalysisCommandPublisher extends RunnerCommandJsonSupport{

  val log: Logger = LoggerFactory.getLogger(getClass)

  val writer = new MqMessageWriter(AnalysisPublisherConfig)


  def main(args: Array[String]): Unit ={

    writer.initialize()

    val startCmd = RunnerCommand("mvn-constant-classes:1.0.0", "test", Set("org.sonatype.tycho:maven-osgi-compiler-plugin", "org.sonatype.tycho:maven-osgi-test-plugin"), "").toJson.compactPrint
    val startCmd2 = RunnerCommand("mvn-dependencies:1.0.0", "test", Set("org.springframework:spring-jms!org.springframework:spring-jms:2.5.6.SEC03", "org.springframework:spring-jms!org.springframework:spring-jms:2.5.6"), "-no-transitive -use-jeka").toJson.compactPrint
    val startCmd3 = RunnerCommand("mvn-constant-classes:1.0.0", "test", Set("com.google.code.gson:gson"), "").toJson.compactPrint
    val startCmd4 = RunnerCommand("mvn-partial-callgraphs:1.0.0", "test", Set("com.google.code.gson:gson!com.google.code.gson:gson:2.9.0"), "").toJson.compactPrint

    log.info(s"Publishing Cmd JSON: $startCmd4")

    writer.appendToQueue(startCmd4)

    log.info("Done Publishing cmd JSON.")

    writer.shutdown()
  }



  object AnalysisPublisherConfig extends MqDirectQueuePublishConfiguration {
    private val prefix = "spa-reuse.mvn-analyses."

    private val akkaConf = ConfigFactory.load()

    override val mqExchangeName: String = akkaConf.getString(prefix + "mq-exchange-name")
    override val mqRoutingKey: String = akkaConf.getString(prefix + "mq-routing-key")
    override val mqMaxPriority: Option[Int] = None
    override val mqUsername: String = akkaConf.getString(prefix + "mq-user")
    override val mqPassword: String = akkaConf.getString(prefix + "mq-pass")
    override val mqHost: String = akkaConf.getString(prefix + "mq-host")
    override val mqPort: Int = akkaConf.getInt(prefix + "mq-port")
    override val mqQueueName: String = akkaConf.getString(prefix + "mq-queue-ident")
    override val mqConnectionName: String = "mvn-analysis-runner-playground"
  }

}


