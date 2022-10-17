package de.tudo.sse.spareuse.execution

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.maven.MavenIdentifier
import de.tudo.sse.spareuse.core.utils.rabbitmq.MqConnectionConfiguration

import scala.util.Try

class AnalysisRunnerConfig private(val mqHost: String,
                                   val mqPort: Int,
                                   val mqUsername: String,
                                   val mqPassword: String,
                                   val mqQueueName: String,
                                   val mqConnectionName: String,
                                   val mavenRepoBase: String,
                                   val executionPoolSize: Int,
                                   val exitOnEnter: Boolean) extends MqConnectionConfiguration


object AnalysisRunnerConfig {

  private final val prefix = "spa-reuse.runner."

  private final val mqUserKey = prefix + "mq-user"
  private final val mqPassKey = prefix + "mq-pass"
  private final val mqHostKey = prefix + "mq-host"
  private final val mqPortKey = prefix + "mq-port"
  private final val mqQueueNameKey = prefix + "mq-queue-ident"
  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val mavenRepoBaseKey = prefix + "maven-repo"

  private final val executionPoolSizeKey = prefix + "exec-pool-size"

  private final val exitOnEnterKey = prefix + "exit-on-enter"

  def fromConfig(c: Config): Try[AnalysisRunnerConfig] = Try {

    if (!c.hasPath(mqUserKey) || !c.hasPath(mqPassKey) || !c.hasPath(mqHostKey) || !c.hasPath(mqPortKey))
      throw new IllegalArgumentException("Message queue configuration incomplete. Required are: User, Pass, Host, Port")

    val mqQueueName = if (c.hasPath(mqQueueNameKey)) c.getString(mqQueueNameKey) else "mvn-library-ident"
    val mqConnectionName = if (c.hasPath(mqConnectionNameSuffixKey)) "AnalysisRunner-" + c.getString(mqConnectionNameSuffixKey) else "MavenEntityMiner"

    val exitOnEnter = if (c.hasPath(exitOnEnterKey)) c.getBoolean(exitOnEnterKey) else true

    val mavenRepoBase = if(c.hasPath(mavenRepoBaseKey)) c.getString(mavenRepoBaseKey) else MavenIdentifier.DefaultRepository

    val executionPoolSize = if (c.hasPath(executionPoolSizeKey)) c.getInt(executionPoolSizeKey) else 1

    new AnalysisRunnerConfig(c.getString(mqHostKey), c.getInt(mqPortKey), c.getString(mqUserKey), c.getString(mqPassKey), mqQueueName,
      mqConnectionName, mavenRepoBase, executionPoolSize, exitOnEnter)

  }

}
