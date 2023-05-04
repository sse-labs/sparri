package org.anon.spareuse.webapi

import com.typesafe.config.ConfigFactory
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success}

object Application {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {

    WebapiConfig.fromConfig(ConfigFactory.load()) match {
      case Success(theConfig) =>
        val theApp = new ClassfileWebApi(theConfig)

        if (theApp.initialize()) {
          theApp.run()
        } else {
          theApp.shutdown()
        }
      case Failure(ex) =>
        log.error("Invalid configuration for webapi", ex)
        System.exit(-1)
    }



  }

}
