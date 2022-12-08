package de.tudo.sse.spareuse.playground

import com.typesafe.config.ConfigFactory
import de.tudo.sse.spareuse.core.model.analysis.{RunnerCommand, StartRunCommand}
import de.tudo.sse.spareuse.core.utils.rabbitmq.{MqDirectQueuePublishConfiguration, MqMessageWriter}
import org.slf4j.{Logger, LoggerFactory}

object AnalysisCommandPublisher {

  val log: Logger = LoggerFactory.getLogger(getClass)

  val writer = new MqMessageWriter(AnalysisPublisherConfig)


  def main(args: Array[String]): Unit ={

    writer.initialize()

    val startCmd = RunnerCommand.toJson(StartRunCommand("mvn-constant-classes:1.0.0", "test", Set("org.sonatype.tycho:maven-osgi-compiler-plugin", "org.sonatype.tycho:maven-osgi-test-plugin"), ""))
    val startCmd2 = RunnerCommand.toJson(StartRunCommand("mvn-dependencies:1.0.0", "test", Set("org.springframework:spring-jms!org.springframework:spring-jms:2.5.6.SEC03", "org.springframework:spring-jms!org.springframework:spring-jms:2.5.6"), "-no-transitive -use-jeka"))
    val startCmd3 = RunnerCommand.toJson(StartRunCommand("mvn-constant-classes:1.0.0", "test", Set("com.google.code.gson:gson"), ""))

    log.info(s"Publishing Cmd JSON: $startCmd3")

    writer.appendToQueue(startCmd3)

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


