package de.tudo.sse.classfilefeatures.common.rabbitmq

import com.rabbitmq.client.{AMQP, Channel, Connection}

import java.util
import scala.util.Try

class MqMessageWriter(configuration: MqConnectionConfiguration) extends MqConnectionBuilder {

  private final val routingKey: String  = ""
  private final val exchangeName: String = "lib-ident-exchange"

  private lazy val connection: Connection = buildConnection(configuration, "classfile-webapi")

  private lazy val channel: Channel = connection.createChannel()

  def initialize(): Unit = {
    val args = new util.HashMap[String, AnyRef]
    args.put("x-max-priority", Integer.valueOf(10))

    channel.exchangeDeclare(exchangeName, "direct", true)
    channel.queueDeclare(configuration.mqQueueName, true, false, false, args)
    channel.queueBind(configuration.mqQueueName, exchangeName, routingKey)
  }

  def appendToQueue(value: String): Unit = {
    val props = new AMQP.BasicProperties.Builder()
      .contentType("text/plain")
      .deliveryMode(2)
      .build()

    channel.basicPublish(exchangeName, routingKey, props, value.getBytes)
  }

  def shutdown(): Unit = Try {
    channel.close()
    connection.close()
  }

}
