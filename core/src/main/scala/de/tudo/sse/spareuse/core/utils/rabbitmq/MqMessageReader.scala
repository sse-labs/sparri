package de.tudo.sse.spareuse.core.utils.rabbitmq

import com.rabbitmq.client.{Channel, Connection}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

class MqMessageReader(configuration: MqConnectionConfiguration, abortOnEmptyQueue: Boolean) extends MqConnectionBuilder {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  val connection: Connection = buildConnection(configuration)

  val channel: Channel = connection.createChannel()

  def readNext(): Option[String] = {
    Try {
      val queueResponse = channel.basicGet(configuration.mqQueueName, false)

      if (queueResponse == null) {
        throw new RuntimeException("No Response from queue")
      } else {
        val messageBody = new String(queueResponse.getBody)
        val messageTag = queueResponse.getEnvelope.getDeliveryTag

        if (messageBody.nonEmpty) {
          channel.basicAck(messageTag, false)
        } else {
          channel.basicNack(messageTag, false, true)
          throw new RuntimeException("Invalid message delivered by queue")
        }

        messageBody
      }
    } match {
      case Success(message) => Some(message)
      case Failure(ex: java.lang.RuntimeException) if ex.getMessage.toLowerCase.contains("no response from queue") =>
        /*if(abortOnEmptyQueue)*/ log.info("No more messages from queue")

        shutdown()

        if(abortOnEmptyQueue) None // None will tell surrounding restart source to stop restarting
        else throw EmptyQueueException(configuration.mqQueueName) // Exception will force a restart in surrounding source

      case Failure(ex) =>
        log.error("Failed to pull library identifier from queue", ex)
        shutdown()
        // Rethrow so surrounding RestartSource can handle ie connection aborts
        throw ex
    }
  }


  def shutdown(): Unit = Try {
    channel.close()
    connection.close()
  }

  case class EmptyQueueException(queueName: String) extends Throwable {
    override def getMessage: String = s"Queue $queueName was empty"
  }

}
