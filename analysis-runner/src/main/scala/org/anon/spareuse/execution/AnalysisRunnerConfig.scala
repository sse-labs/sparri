package org.anon.spareuse.execution

import com.typesafe.config.Config
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.utils.rabbitmq.{MqConnectionConfiguration, MqDirectQueuePublishConfiguration}

import scala.util.Try

class AnalysisRunnerConfig private(val mqHostAdr: String,
                                   val mqPortNum: Int,
                                   val mqUser: String,
                                   val mqPass: String,
                                   val analysisQueueName: String,
                                   val mqConnectionName: String,
                                   val entityQueueName: String,
                                   val entityExchangeName: String,
                                   val entityRoutingKey: String,
                                   val entityMaxPriority: Option[Int],
                                   val mavenRepoBase: String,
                                   val executionPoolSize: Int,
                                   val exitOnEnter: Boolean) extends MqConnectionConfiguration {

  override val mqQueueName: String = analysisQueueName
  override val mqUsername: String = mqUser
  override val mqPassword: String = mqPass
  override val mqPort: Int = mqPortNum
  override val mqHost: String = mqHostAdr

  def toWriterConfig: MqDirectQueuePublishConfiguration = new MqDirectQueuePublishConfiguration {
    override val mqExchangeName: String = entityExchangeName
    override val mqRoutingKey: String = entityRoutingKey
    override val mqMaxPriority: Option[Int] = entityMaxPriority
    override val mqUsername: String = mqUser
    override val mqPassword: String = mqPass
    override val mqHost: String = mqHostAdr
    override val mqPort: Int = mqPortNum
    override val mqQueueName: String = entityQueueName
    override val mqConnectionName: String = "AnalysisRunner-OnDemand"
  }
}


object AnalysisRunnerConfig {

  private final val prefix = "spa-reuse.runner."

  private final val mqUserKey = prefix + "mq-user"
  private final val mqPassKey = prefix + "mq-pass"
  private final val mqHostKey = prefix + "mq-host"
  private final val mqPortKey = prefix + "mq-port"
  private final val mqQueueNameKey = prefix + "mq-queue-ident"
  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val entityQueueNameKey = prefix + "entity-queue-name"
  private final val entityExchangeNameKey = prefix + "entity-exchange-name"
  private final val entityRoutingKey = prefix + "entity-routing-key"
  private final val entityMaxPriorityKey = prefix + "entity-max-priority"


  private final val mavenRepoBaseKey = prefix + "maven-repo"

  private final val executionPoolSizeKey = prefix + "exec-pool-size"

  private final val exitOnEnterKey = prefix + "exit-on-enter"

  def fromConfig(c: Config): Try[AnalysisRunnerConfig] = Try {

    if (!c.hasPath(mqUserKey) || !c.hasPath(mqPassKey) || !c.hasPath(mqHostKey) || !c.hasPath(mqPortKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are: User, Pass, Host, Port")

    val mqQueueName = if (c.hasPath(mqQueueNameKey)) c.getString(mqQueueNameKey) else "mvn-library-ident"
    val mqConnectionName = if (c.hasPath(mqConnectionNameSuffixKey)) "AnalysisRunner-" + c.getString(mqConnectionNameSuffixKey) else "AnalysisRunner"

    if(!c.hasPath(entityQueueNameKey) || !c.hasPath(entityExchangeNameKey) || !c.hasPath(entityRoutingKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are entity-queue-name, exchange name and routing key")


    val entityMaxPrio = if (c.hasPath(entityMaxPriorityKey)) Some(c.getInt(entityMaxPriorityKey)) else None

    val exitOnEnter = if (c.hasPath(exitOnEnterKey)) c.getBoolean(exitOnEnterKey) else true

    val mavenRepoBase = if(c.hasPath(mavenRepoBaseKey)) c.getString(mavenRepoBaseKey) else MavenIdentifier.DefaultRepository

    val executionPoolSize = if (c.hasPath(executionPoolSizeKey)) c.getInt(executionPoolSizeKey) else 1

    new AnalysisRunnerConfig(c.getString(mqHostKey), c.getInt(mqPortKey), c.getString(mqUserKey), c.getString(mqPassKey), mqQueueName,
      mqConnectionName, c.getString(entityQueueNameKey), c.getString(entityExchangeNameKey), c.getString(entityRoutingKey),
      entityMaxPrio, mavenRepoBase, executionPoolSize, exitOnEnter)

  }

}
