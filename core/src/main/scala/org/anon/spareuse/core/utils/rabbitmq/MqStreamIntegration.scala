package org.anon.spareuse.core.utils.rabbitmq

import akka.NotUsed
import akka.event.Logging
import akka.event.Logging.LogLevel
import akka.stream.RestartSettings
import akka.stream.scaladsl.{RestartSource, Source}
import org.slf4j.{Logger, LoggerFactory}

import java.time.Duration
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}


trait MqStreamIntegration {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def createMqMessageSource(config: MqConnectionConfiguration, abortOnEmptyQueue: Boolean): Source[String, NotUsed] = {

    var sourceSettings = RestartSettings.create(minBackoff = Duration.ofSeconds(10),
      maxBackoff = Duration.ofSeconds(10), randomFactor = 0.2)
      .withMaxRestarts(1000, 5.minutes) // Always restart and never cancel

    if(!abortOnEmptyQueue) // Suppress error output when we do not abort on empty queue -> Don't print EmptyQueueExceptions
      sourceSettings = sourceSettings.withLogSettings(sourceSettings.logSettings.withLogLevel(Logging.DebugLevel))

    RestartSource.withBackoff(sourceSettings){ () =>

      Try(new MqMessageReader(config, abortOnEmptyQueue)) match {

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
