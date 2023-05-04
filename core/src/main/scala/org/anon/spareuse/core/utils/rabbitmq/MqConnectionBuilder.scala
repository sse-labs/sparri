package org.anon.spareuse.core.utils.rabbitmq

import com.rabbitmq.client.{Connection, ConnectionFactory}

object MqConnectionBuilder {

  def buildConnection(configuration: MqConnectionConfiguration): Connection = {

    val factory = new ConnectionFactory

    factory.setUsername(configuration.mqUsername)
    factory.setPassword(configuration.mqPassword)
    factory.setVirtualHost("/")
    factory.setHost(configuration.mqHost)
    factory.setPort(configuration.mqPort)

    factory.newConnection(configuration.mqConnectionName)

  }

}

trait MqConnectionConfiguration {

  val mqUsername: String
  val mqPassword: String
  val mqHost: String
  val mqPort: Int
  val mqQueueName: String
  val mqConnectionName: String

}

trait MqDirectQueuePublishConfiguration extends MqConnectionConfiguration {

  val mqExchangeName: String
  val mqRoutingKey: String
  val mqMaxPriority: Option[Int]

}
