package org.anon.spareuse.core.utils.rabbitmq

import com.typesafe.config.{Config, ConfigFactory}

object MqConfigurationBuilder {

  private val keyPrefix = "spa-reuse.mq."
  private val userKey = keyPrefix + "mq-user"
  private val passKey = keyPrefix + "mq-pass"
  private val hostKey = keyPrefix + "mq-host"
  private val portKey = keyPrefix + "mq-port"

  private val entityQNameKey = keyPrefix + "entity-queue-name"
  private val entityQRoutingKey = keyPrefix + "entity-queue-routing-key"
  private val entityQExchangeNameKey = keyPrefix + "entity-queue-exchange-name"
  private val entityQPrioKey = keyPrefix + "entity-queue-max-priority"

  private val analysisQNameKey = keyPrefix + "analysis-queue-name"
  private val analysisQRoutingKey = keyPrefix + "analysis-queue-routing-key"
  private val analysisQExchangeNameKey = keyPrefix + "analysis-queue-exchange-name"
  private val analysisQPrioKey = keyPrefix + "analysis-queue-max-priority"

  private val requiredKeys = Set(userKey, passKey, hostKey, portKey)

  type BasicConnectionInfo = (String, String, String, Int)

  private def tryGetEnvOrElse(envName: String, elseProducer: () => String): String = {
    val envValue = System.getenv(envName)

    if(envValue == null) elseProducer.apply()
    else envValue
  }

  private def getBasics(conf: Config): BasicConnectionInfo = {
    if(requiredKeys.exists(key => !conf.hasPath(key)))
      throw new IllegalArgumentException(s"MQ Configuration is missing a required member: ${requiredKeys.mkString(",")}")

    (
      tryGetEnvOrElse("SPARRI_MQ_USER", () => conf.getString(userKey)),
      tryGetEnvOrElse("SPARRI_MQ_PASS", () => conf.getString(passKey)),
      tryGetEnvOrElse("SPARRI_MQ_HOST", () => conf.getString(hostKey)),
      if(System.getenv("SPARRI_MQ_PORT") != null) System.getenv("SPARRI_MQ_PORT").toInt else conf.getInt(portKey)
    )
  }

  private def buildReadConfig(queueName: String, connectionName: String, basics: BasicConnectionInfo): MqConnectionConfiguration = new MqConnectionConfiguration {
    override val mqUsername: String = basics._1
    override val mqPassword: String = basics._2
    override val mqHost: String = basics._3
    override val mqPort: Int = basics._4
    override val mqQueueName: String = queueName
    override val mqConnectionName: String = connectionName
  }

  private def buildWriteConfig(exName: String, routingKey: String, priorities: Option[Int], readConfig: MqConnectionConfiguration): MqDirectQueuePublishConfiguration = new MqDirectQueuePublishConfiguration {
    override val mqExchangeName: String = exName
    override val mqRoutingKey: String = routingKey
    override val mqMaxPriority: Option[Int] = priorities
    override val mqUsername: String = readConfig.mqUsername
    override val mqPassword: String = readConfig.mqPassword
    override val mqHost: String = readConfig.mqHost
    override val mqPort: Int = readConfig.mqPort
    override val mqQueueName: String = readConfig.mqQueueName
    override val mqConnectionName: String = readConfig.mqConnectionName
  }

  def entityReadConfig(connectionName: String): MqConnectionConfiguration = {
    val configObj = ConfigFactory.load()
    val basics = getBasics(configObj)

    val entityQueueName = if(configObj.hasPath(entityQNameKey)) configObj.getString(entityQNameKey) else "mvn-library-names"

    buildReadConfig(entityQueueName, connectionName, basics)
  }

  def entityWriteConfig(connectionName: String): MqDirectQueuePublishConfiguration = {
    val readConfig = entityReadConfig(connectionName)

    val configObj = ConfigFactory.load()

    val routingKey = if(configObj.hasPath(entityQRoutingKey))configObj.getString(entityQRoutingKey) else "mvn"
    val exchangeName = if(configObj.hasPath(entityQExchangeNameKey)) configObj.getString(entityQExchangeNameKey) else "library-name-exchange"
    val priorities = if(configObj.hasPath(entityQPrioKey)) Some(configObj.getInt(entityQPrioKey)) else None

    buildWriteConfig(exchangeName, routingKey, priorities, readConfig)
  }

  def analysisReadConfig(connectionName: String): MqConnectionConfiguration = {
    val configObj = ConfigFactory.load()
    val basics = getBasics(configObj)

    val entityQueueName = if (configObj.hasPath(analysisQNameKey)) configObj.getString(analysisQNameKey) else "mvn-analyses"

    buildReadConfig(entityQueueName, connectionName, basics)
  }

  def analysisWriteConfig(connectionName: String): MqDirectQueuePublishConfiguration = {
    val readConfig = analysisReadConfig(connectionName)

    val configObj = ConfigFactory.load()

    val routingKey = if (configObj.hasPath(analysisQRoutingKey)) configObj.getString(analysisQRoutingKey) else "mvn"
    val exchangeName = if (configObj.hasPath(analysisQExchangeNameKey)) configObj.getString(analysisQExchangeNameKey) else "analyses-exchange"
    val priorities = if (configObj.hasPath(analysisQPrioKey)) Some(configObj.getInt(analysisQPrioKey)) else None

    buildWriteConfig(exchangeName, routingKey, priorities, readConfig)
  }

}
