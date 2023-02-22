package de.tudo.sse.classfilefeatures.webapi

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqDirectQueuePublishConfiguration

import scala.util.Try

class WebapiConfig private(val bindHost: String,
                           val bindPort: Int,
                           val mqHostAdr: String,
                           val mqPortNum: Int,
                           val mqUser: String,
                           val mqPass: String,
                           val analysisQueueName: String,
                           val analysisQueueConnectionName: String,
                           val analysisExchangeName: String,
                           val analysisRoutingKey: String,
                           val entityQueueName: String,
                           val entityQueueConnectionName: String,
                           val entityExchangeName: String,
                           val entityRoutingKey: String,
                           val entityMaxPriority: Option[Int]) {
  def asMinerQueuePublishConfig: MqDirectQueuePublishConfiguration = new MqDirectQueuePublishConfiguration {
    override val mqExchangeName: String = entityExchangeName
    override val mqRoutingKey: String = entityRoutingKey
    override val mqMaxPriority: Option[Int] = entityMaxPriority
    override val mqUsername: String = mqUser
    override val mqPassword: String = mqPass
    override val mqHost: String = mqHostAdr
    override val mqPort: Int = mqPortNum
    override val mqQueueName: String = entityQueueName
    override val mqConnectionName: String = entityQueueConnectionName
  }
  def asAnalysisQueuePublishConfig: MqDirectQueuePublishConfiguration = new MqDirectQueuePublishConfiguration {
    override val mqExchangeName: String = analysisExchangeName
    override val mqRoutingKey: String = analysisRoutingKey
    override val mqMaxPriority: Option[Int] = None
    override val mqUsername: String = mqUser
    override val mqPassword: String = mqPass
    override val mqHost: String = mqHostAdr
    override val mqPort: Int = mqPortNum
    override val mqQueueName: String = analysisQueueName
    override val mqConnectionName: String = analysisQueueConnectionName
  }
}

object WebapiConfig {
  private final val prefix = "spa-reuse.webapi."

  private final val bindHostKey = prefix + "bind-host"
  private final val bindPortKey = prefix + "bind-port"

  private final val mqUserKey = prefix + "mq-user"
  private final val mqPassKey = prefix + "mq-pass"
  private final val mqHostKey = prefix + "mq-host"
  private final val mqPortKey = prefix + "mq-port"

  private final val analysisQNameKey = prefix + "analysis-queue-name"
  private final val analysisQConnKey = prefix + "analysis-queue-connection-name"
  private final val analysisQExchangeKey = prefix + "analysis-queue-exchange-name"
  private final val analysisQRoutingKey = prefix + "analysis-queue-routing-key"

  private final val entityQNameKey = prefix + "entity-queue-name"
  private final val entityQConnKey = prefix + "entity-queue-connection-name"
  private final val entityQExchangeKey = prefix + "entity-queue-exchange-name"
  private final val entityQRoutingKey = prefix + "entity-queue-routing-key"
  private final val entityQPrioKey = prefix + "entity-queue-max-priority"

  def fromConfig(c: Config): Try[WebapiConfig] = Try {
    if(!c.hasPath(bindHostKey) || !c.hasPath(bindPortKey))
      throw new IllegalArgumentException("Webapi configuration incomplete. Required are: bind-host, bind-port")

    if (!c.hasPath(mqUserKey) || !c.hasPath(mqPassKey) || !c.hasPath(mqHostKey) || !c.hasPath(mqPortKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are: User, Pass, Host, Port")

    if(!c.hasPath(analysisQNameKey) || !c.hasPath(analysisQExchangeKey) || !c.hasPath(analysisQRoutingKey))
      throw new IllegalArgumentException("Analysis queue configuration incomplete. Required are: name, exchange, routing-key")

    val analysisConnName = if(c.hasPath(analysisQConnKey)) c.getString(analysisQConnKey) else "spar-webapi"

    if(!c.hasPath(entityQNameKey) || !c.hasPath(entityQExchangeKey) || !c.hasPath(entityQRoutingKey))
      throw new IllegalArgumentException("Entity queue configuration incomplete. Required are: name, exchange, routing-key")

    val entityConnName = if(c.hasPath(entityQConnKey)) c.getString(entityQConnKey) else "spar-webapi"
    val maxPrio = if(c.hasPath(entityQPrioKey)) Some(c.getInt(entityQPrioKey)) else None


    new WebapiConfig(
      c.getString(bindHostKey),
      c.getInt(bindPortKey),
      c.getString(mqHostKey),
      c.getInt(mqPortKey),
      c.getString(mqUserKey),
      c.getString(mqPassKey),
      c.getString(analysisQNameKey),
      analysisConnName,
      c.getString(analysisQExchangeKey),
      c.getString(analysisQRoutingKey),
      c.getString(entityQNameKey),
      entityConnName,
      c.getString(entityQExchangeKey),
      c.getString(entityQRoutingKey),
      maxPrio)
  }

}
