package de.tudo.sse.spareuse.core.utils.rabbitmq

import akka.NotUsed
import akka.stream.RestartSettings
import akka.stream.scaladsl.{RestartSource, Source}
import org.slf4j.{Logger, LoggerFactory}

import java.time.Duration
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}


trait MqStreamIntegration {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def createMqMessageSource(config: MqConnectionConfiguration): Source[String, NotUsed] = {

    val sourceSettings = RestartSettings.create(minBackoff = Duration.ofSeconds(30),
      maxBackoff = Duration.ofSeconds(120), randomFactor = 0.2)
      .withMaxRestarts(10, 10.minutes)

    RestartSource.onFailuresWithBackoff(sourceSettings){ () =>

      Try(new MqMessageReader(config)) match {

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
