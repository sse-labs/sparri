package de.tudo.sse.spareuse.mvnpub

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.maven.MavenIdentifier
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqDirectQueuePublishConfiguration

import scala.util.Try

class MavenEntityPublisherConfig private(val mqHost: String,
                                         val mqPort: Int,
                                         val mqUsername: String,
                                         val mqPassword: String,
                                         val mqQueueName: String,
                                         val mqConnectionName: String,
                                         val mqExchangeName:String,
                                         val mqRoutingKey: String,
                                         val mqMaxPriority: Option[Int],
                                         val mavenRepoBase: String) extends MqDirectQueuePublishConfiguration

object MavenEntityPublisherConfig {

  private final val prefix = "spa-reuse.mvn-pub."

  private final val mqUserKey = prefix + "mq-user"
  private final val mqPassKey = prefix + "mq-pass"
  private final val mqHostKey = prefix + "mq-host"
  private final val mqPortKey = prefix + "mq-port"
  private final val mqQueueNameKey = prefix + "mq-queue-ident"
  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"
  private final val mqExchangeNameKey = prefix + "mq-exchange-name"
  private final val mqRoutingKey = prefix + "mq-routing-key"
  private final val mqMaxPriorityKey = prefix + "mq-max-priority"

  private final val mvnRepoBaseKey = prefix + "mvn-repo-url"


  def fromConfig(c: Config): Try[MavenEntityPublisherConfig] = Try {

    if(!c.hasPath(mqUserKey) || !c.hasPath(mqPassKey) || !c.hasPath(mqHostKey) || !c.hasPath(mqPortKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are: User, Pass, Host, Port")

    val mqQueueName = if(c.hasPath(mqQueueNameKey)) c.getString(mqQueueNameKey) else "mvn-library-ident"
    val mqConnectionName= if(c.hasPath(mqConnectionNameSuffixKey)) "MavenEntityPublisher-" + c.getString(mqConnectionNameSuffixKey) else "MavenEntityPublisher"

    if(!c.hasPath(mqExchangeNameKey) || !c.hasPath(mqRoutingKey))
      throw new IllegalArgumentException("Message queue publisher configuration incomplete. Required are: ExchangeName, RoutingKey")

    val mqMaxPriorityOpt = if(c.hasPath(mqMaxPriorityKey)) Some(c.getInt(mqMaxPriorityKey)) else None

    val mvnRepoBase = if(c.hasPath(mvnRepoBaseKey)) c.getString(mvnRepoBaseKey) else MavenIdentifier.DefaultRepository

    new MavenEntityPublisherConfig(c.getString(mqHostKey), c.getInt(mqPortKey), c.getString(mqUserKey), c.getString(mqPassKey), mqQueueName,
      mqConnectionName, c.getString(mqExchangeNameKey), c.getString(mqRoutingKey), mqMaxPriorityOpt, mvnRepoBase)

  }

}

