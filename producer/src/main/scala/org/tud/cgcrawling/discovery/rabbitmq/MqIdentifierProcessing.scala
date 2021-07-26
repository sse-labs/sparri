package org.tud.cgcrawling.discovery.rabbitmq

import akka.NotUsed
import akka.actor.ActorSystem
import akka.japi.Creator
import akka.stream.scaladsl.{RestartSource, Source}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait MqIdentifierProcessing {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def createSource(configuration: Configuration) (implicit system: ActorSystem): Source[String, NotUsed] = {

    RestartSource.withBackoff(minBackoff = 30.seconds,
      maxBackoff = 120.seconds,
      randomFactor = 0.2,
      maxRestarts = 50){ () => {
      Try(new MqMessageReader(configuration)) match {

        case Success(reader) =>
          Source.unfoldResource[String, MqMessageReader](
            () => reader,
            r => r.readNext(),
            r => r.shutdown())

        case Failure(ex) =>
          log.error("Failed build source from MQ", ex)
          throw ex
      }
    }
    }
  }
}
