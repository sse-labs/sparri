package de.tudo.sse.spareuse.mvnem

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.rabbitmq.{MqConnectionConfiguration, MqDirectQueuePublishConfiguration}

import scala.util.Try

class EntityMinerConfig private(val mqHostAdr: String,
                                val mqPortNum: Int,
                                val mqUser: String,
                                val mqPass: String,
                                val mqQueueName: String,
                                val mqConnectionName: String,
                                val analysisQueueName: String,
                                val analysisExchangeName: String,
                                val analysisRoutingKey: String,
                                val storageParallel: Int,
                                val exitOnEnter: Boolean) extends MqConnectionConfiguration {

  override val mqUsername: String = mqUser
  override val mqPassword: String = mqPass
  override val mqPort: Int = mqPortNum
  override val mqHost: String = mqHostAdr

  def toWriterConfig: MqDirectQueuePublishConfiguration = new MqDirectQueuePublishConfiguration {
    override val mqExchangeName: String = analysisExchangeName
    override val mqRoutingKey: String = analysisRoutingKey
    override val mqMaxPriority: Option[Int] = None
    override val mqUsername: String = mqUser
    override val mqPassword: String = mqPass
    override val mqHost: String = mqHostAdr
    override val mqPort: Int = mqPortNum
    override val mqQueueName: String = analysisQueueName
    override val mqConnectionName: String = "EntityMiner-AnalysesRequeue"
  }

}

object EntityMinerConfig {

  private final val prefix = "spa-reuse.mvn-em."

  private final val mqUserKey = prefix + "mq-user"
  private final val mqPassKey = prefix + "mq-pass"
  private final val mqHostKey = prefix + "mq-host"
  private final val mqPortKey = prefix + "mq-port"
  private final val mqQueueNameKey = prefix + "mq-queue-ident"
  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val analysisQueueNameKey = prefix + "analysis-queue-name"
  private final val analysisExchangeKey = prefix + "analysis-exchange-name"
  private final val analysisRoutingKey = prefix + "analysis-routing-key"

  private final val storageParallelKey = prefix + "storage-parallel"

  private final val exitOnEnterKey = prefix + "exit-on-enter"


  def fromConfig(c: Config): Try[EntityMinerConfig] = Try {

    if(!c.hasPath(mqUserKey) || !c.hasPath(mqPassKey) || !c.hasPath(mqHostKey) || !c.hasPath(mqPortKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are: User, Pass, Host, Port")

    val mqQueueName = if(c.hasPath(mqQueueNameKey)) c.getString(mqQueueNameKey) else "mvn-library-ident"
    val mqConnectionName= if(c.hasPath(mqConnectionNameSuffixKey)) "MavenEntityMiner-" + c.getString(mqConnectionNameSuffixKey) else "MavenEntityMiner"
    val storageParallel = if(c.hasPath(storageParallelKey)) c.getInt(storageParallelKey) else 1

    val exitOnEnter = if(c.hasPath(exitOnEnterKey)) c.getBoolean(exitOnEnterKey) else true

    if(!c.hasPath(analysisQueueNameKey) || !c.hasPath(analysisExchangeKey) || !c.hasPath(analysisRoutingKey))
      throw new IllegalArgumentException("Analysis queue configuration incomplete. Required are: Queue, Exchange, RoutingKey")

    new EntityMinerConfig(c.getString(mqHostKey), c.getInt(mqPortKey), c.getString(mqUserKey), c.getString(mqPassKey), mqQueueName,
      mqConnectionName, c.getString(analysisQueueNameKey), c.getString(analysisExchangeKey), c.getString(analysisRoutingKey),
      storageParallel, exitOnEnter)

  }

}
