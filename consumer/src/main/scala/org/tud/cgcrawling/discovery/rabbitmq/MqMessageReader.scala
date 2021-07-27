package org.tud.cgcrawling.discovery.rabbitmq

import akka.actor.ActorSystem
import com.rabbitmq.client.{Channel, Connection, ConnectionFactory}
import org.tud.cgcrawling.{AppLogging, Configuration}

import scala.util.{Failure, Success, Try}

class MqMessageReader(configuration: Configuration)(implicit system: ActorSystem) extends AppLogging {

  lazy val connection: Connection = {
    val factory = new ConnectionFactory

    factory.setUsername(configuration.mqUsername)
    factory.setPassword(configuration.mqPassword)
    factory.setVirtualHost("/")
    factory.setHost(configuration.mqHost)
    factory.setPort(configuration.mqPort)

    factory.newConnection("incremental-cg-crawler-consumer")
  }

  lazy val channel: Channel = connection.createChannel()

  def readNext(): Option[String] = {
    Try {
      val queueResponse = channel.basicGet(configuration.mqQueueName, false)

      if (queueResponse == null) {
        throw new RuntimeException("No Response from queue")
      } else {
        val messageBody = new String(queueResponse.getBody)
        val messageTag = queueResponse.getEnvelope.getDeliveryTag

        if (messageBody.nonEmpty && messageBody.contains(":")) {
          channel.basicAck(messageTag, false)
        } else {
          channel.basicNack(messageTag, false, true)
          throw new RuntimeException("Invalid message delivered by queue")
        }

        messageBody
      }
    } match {
      case Success(libraryIdent) => Some(libraryIdent)
      case Failure(ex: java.lang.RuntimeException) if ex.getMessage.toLowerCase.contains("no response from queue") =>
        log.info("No more messages from queue")
        // Returning None will tell the surrounding RestartSource that this stream has completed
        shutdown()
        None
      case Failure(ex) =>
        log.error(ex, "Failed to pull library identifier from queue")
        shutdown()
        // Rethrow so surrounding RestartSource can handle ie connection aborts
        throw ex
    }
  }


  def shutdown(): Unit = Try {
    channel.close()
    connection.close()
  }

}
