package de.tudo.sse.classfilefeatures.webapi

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqDirectQueuePublishConfiguration

import scala.util.Try

class WebapiConfig private(val bindHost: String,
                           val bindPort: Int) {
  def asMinerQueuePublishConfig: MqDirectQueuePublishConfiguration = ???
  def asAnalysisQueuePublishConfig: MqDirectQueuePublishConfiguration = ???
}

object WebapiConfig {
  private final val prefix = "spa-reuse.webapi."

  private final val bindHostKey = prefix + "bind-host"
  private final val bindPortKey = prefix + "bind-port"

  def fromConfig(c: Config): Try[WebapiConfig] = Try {
    if(!c.hasPath(bindHostKey) || !c.hasPath(bindPortKey))
      throw new IllegalArgumentException("Webapi configuration incomplete. Required are: bind-host, bind-port")

    new WebapiConfig(c.getString(bindHostKey), c.getInt(bindPortKey))
  }

}
