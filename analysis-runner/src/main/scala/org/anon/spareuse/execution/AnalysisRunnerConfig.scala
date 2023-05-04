package org.anon.spareuse.execution

import com.typesafe.config.Config
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.utils.rabbitmq.{MqConfigurationBuilder, MqConnectionConfiguration, MqDirectQueuePublishConfiguration}
import org.anon.spareuse.execution.AnalysisRunnerConfig.mqConnectionNameSuffixKey

import scala.util.Try

class AnalysisRunnerConfig private(val mqConnectionName: String,
                                   val executionPoolSize: Int,
                                   val exitOnEnter: Boolean) {


  def toReaderConfig: MqConnectionConfiguration = MqConfigurationBuilder.analysisReadConfig(mqConnectionName)
  def toWriterConfig: MqDirectQueuePublishConfiguration = MqConfigurationBuilder.entityWriteConfig("AnalysisRunner-OnDemand")

}


object AnalysisRunnerConfig {

  private final val prefix = "spa-reuse.runner."
  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val executionPoolSizeKey = prefix + "exec-pool-size"

  private final val exitOnEnterKey = prefix + "exit-on-enter"

  def fromConfig(c: Config): Try[AnalysisRunnerConfig] = Try {

    val mqConnectionName = if (c.hasPath(mqConnectionNameSuffixKey)) "AnalysisRunner-" + c.getString(mqConnectionNameSuffixKey) else "AnalysisRunner"

    val exitOnEnter = if (c.hasPath(exitOnEnterKey)) c.getBoolean(exitOnEnterKey) else true


    val executionPoolSize = if (c.hasPath(executionPoolSizeKey)) c.getInt(executionPoolSizeKey) else 1

    new AnalysisRunnerConfig(mqConnectionName, executionPoolSize, exitOnEnter)
  }

}
