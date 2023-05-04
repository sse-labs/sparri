package org.anon.spareuse.mvnem

import com.typesafe.config.Config
import org.anon.spareuse.core.utils.rabbitmq.{MqConfigurationBuilder, MqConnectionConfiguration, MqDirectQueuePublishConfiguration}

import scala.util.Try

class EntityMinerConfig private(val mqConnectionName: String,
                                val storageParallel: Int,
                                val exitOnEnter: Boolean) {
  def toReadConfig: MqConnectionConfiguration = MqConfigurationBuilder.entityReadConfig(mqConnectionName)

  def toWriterConfig: MqDirectQueuePublishConfiguration = MqConfigurationBuilder.analysisWriteConfig("EntityMiner-AnalysesRequeue")

}

object EntityMinerConfig {

  private final val prefix = "spa-reuse.mvn-em."

  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val storageParallelKey = prefix + "storage-parallel"

  private final val exitOnEnterKey = prefix + "exit-on-enter"


  def fromConfig(c: Config): Try[EntityMinerConfig] = Try {

    val mqConnectionName= if(c.hasPath(mqConnectionNameSuffixKey)) "MavenEntityMiner-" + c.getString(mqConnectionNameSuffixKey) else "MavenEntityMiner"
    val storageParallel = if(c.hasPath(storageParallelKey)) c.getInt(storageParallelKey) else 1

    val exitOnEnter = if(c.hasPath(exitOnEnterKey)) c.getBoolean(exitOnEnterKey) else true

    new EntityMinerConfig(mqConnectionName, storageParallel, exitOnEnter)

  }

}
