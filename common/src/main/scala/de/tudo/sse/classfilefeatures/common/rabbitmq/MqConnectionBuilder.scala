package de.tudo.sse.classfilefeatures.common.rabbitmq

import com.rabbitmq.client.{Connection, ConnectionFactory}

trait MqConnectionBuilder {

  def buildConnection(configuration: MqConnectionConfiguration, connectionName: String): Connection = {

    val factory = new ConnectionFactory

    factory.setUsername(configuration.mqUsername)
    factory.setPassword(configuration.mqPassword)
    factory.setVirtualHost("/")
    factory.setHost(configuration.mqHost)
    factory.setPort(configuration.mqPort)

    factory.newConnection(connectionName)

  }

}

trait MqConnectionConfiguration {

  val mqUsername: String
  val mqPassword: String
  val mqHost: String
  val mqPort: Int
  val mqQueueName: String

}
