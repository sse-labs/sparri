package de.tudo.sse.spareuse.mvnem

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqConnectionConfiguration

import scala.util.Try

class EntityMinerConfig private(val mqHost: String,
                                val mqPort: Int,
                                val mqUsername: String,
                                val mqPassword: String,
                                val mqQueueName: String,
                                val mqConnectionName: String,
                                val storageParallel: Int) extends MqConnectionConfiguration

object EntityMinerConfig {

  private final val prefix = "spa-reuse.mvn-em."

  private final val mqUserKey = prefix + "mq-user"
  private final val mqPassKey = prefix + "mq-pass"
  private final val mqHostKey = prefix + "mq-host"
  private final val mqPortKey = prefix + "mq-port"
  private final val mqQueueNameKey = prefix + "mq-queue-ident"
  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val storageParallelKey = prefix + "storage-parallel"


  def fromConfig(c: Config): Try[EntityMinerConfig] = Try {

    if(!c.hasPath(mqUserKey) || !c.hasPath(mqPassKey) || !c.hasPath(mqHostKey) || !c.hasPath(mqPortKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are: User, Pass, Host, Port")

    val mqQueueName = if(c.hasPath(mqQueueNameKey)) c.getString(mqQueueNameKey) else "mvn-library-ident"
    val mqConnectionName= if(c.hasPath(mqConnectionNameSuffixKey)) "MavenEntityMiner-" + c.getString(mqConnectionNameSuffixKey) else "MavenEntityMiner"
    val storageParallel = if(c.hasPath(storageParallelKey)) c.getInt(storageParallelKey) else 1

    new EntityMinerConfig(c.getString(mqHostKey), c.getInt(mqPortKey), c.getString(mqUserKey), c.getString(mqPassKey), mqQueueName,
      mqConnectionName, storageParallel)

  }

}
