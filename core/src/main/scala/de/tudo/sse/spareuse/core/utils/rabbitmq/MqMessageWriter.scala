package de.tudo.sse.spareuse.core.utils.rabbitmq

import com.rabbitmq.client.{AMQP, Channel, Connection}

import java.util
import scala.util.Try

class MqMessageWriter(configuration: MqDirectQueuePublishConfiguration) extends MqConnectionBuilder {

  private lazy val connection: Connection = buildConnection(configuration)

  private lazy val channel: Channel = connection.createChannel()

  def initialize(): Unit = {
    val args = new util.HashMap[String, AnyRef]

    if(configuration.mqMaxPriority.isDefined)
      args.put("x-max-priority", Integer.valueOf(configuration.mqMaxPriority.get))

    channel.exchangeDeclare(configuration.mqExchangeName, "direct", true)
    channel.queueDeclare(configuration.mqQueueName, true, false, false, args)
    channel.queueBind(configuration.mqQueueName, configuration.mqExchangeName, configuration.mqRoutingKey)
  }

  def appendToQueue(value: String, priority: Option[Int] = None): Unit = {
    val props = new AMQP.BasicProperties.Builder()
      .contentType("text/plain")
      .priority(Integer.valueOf(priority.getOrElse(0)))
      .deliveryMode(2)
      .build()

    channel.basicPublish(configuration.mqExchangeName, configuration.mqRoutingKey, props, value.getBytes)
  }

  def shutdown(): Unit = Try {
    channel.close()
    connection.close()
  }

}
