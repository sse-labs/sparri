package org.anon.spareuse.webapi

import com.typesafe.config.Config
import org.anon.spareuse.core.utils.rabbitmq.{MqConfigurationBuilder, MqDirectQueuePublishConfiguration}

import scala.util.Try

class WebapiConfig private(val bindHost: String,
                           val bindPort: Int,
                           val analysisQueueConnectionName: String,
                           val entityQueueConnectionName: String) {
  def asMinerQueuePublishConfig: MqDirectQueuePublishConfiguration = MqConfigurationBuilder.entityWriteConfig(entityQueueConnectionName)
  def asAnalysisQueuePublishConfig: MqDirectQueuePublishConfiguration = MqConfigurationBuilder.analysisWriteConfig(analysisQueueConnectionName)
}

object WebapiConfig {
  private final val prefix = "spa-reuse.webapi."

  private final val bindHostKey = prefix + "bind-host"
  private final val bindPortKey = prefix + "bind-port"

  private final val analysisQConnKey = prefix + "analysis-queue-connection-name"
  private final val entityQConnKey = prefix + "entity-queue-connection-name"

  def fromConfig(c: Config): Try[WebapiConfig] = Try {
    if(!c.hasPath(bindHostKey) || !c.hasPath(bindPortKey))
      throw new IllegalArgumentException("Webapi configuration incomplete. Required are: bind-host, bind-port")

    val analysisConnName = if(c.hasPath(analysisQConnKey)) c.getString(analysisQConnKey) else "spar-webapi"

    val entityConnName = if(c.hasPath(entityQConnKey)) c.getString(entityQConnKey) else "spar-webapi"

    new WebapiConfig(c.getString(bindHostKey), c.getInt(bindPortKey), analysisConnName, entityConnName)
  }

}
