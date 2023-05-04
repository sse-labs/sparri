package org.anon.spareuse.mvnpub

import com.typesafe.config.Config
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.utils.rabbitmq.{MqConfigurationBuilder, MqDirectQueuePublishConfiguration}

import scala.util.Try

class MavenEntityPublisherConfig private(val mqConnectionName: String,
                                         val mavenRepoBase: String){
  def buildEntityQueueConfiguration: MqDirectQueuePublishConfiguration = MqConfigurationBuilder.entityWriteConfig(mqConnectionName)
}

object MavenEntityPublisherConfig {

  private final val prefix = "spa-reuse.mvn-pub."

  private final val mqConnectionNameSuffixKey = prefix + "mq-conn-name-suffix"

  private final val mvnRepoBaseKey = prefix + "mvn-repo-url"


  def fromConfig(c: Config): Try[MavenEntityPublisherConfig] = Try {
    val mqConnectionName= if(c.hasPath(mqConnectionNameSuffixKey)) "MavenEntityPublisher-" + c.getString(mqConnectionNameSuffixKey) else "MavenEntityPublisher"

    val mvnRepoBase = if(c.hasPath(mvnRepoBaseKey)) c.getString(mvnRepoBaseKey) else MavenIdentifier.DefaultRepository

    new MavenEntityPublisherConfig(mqConnectionName, mvnRepoBase)

  }

}

