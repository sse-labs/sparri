package de.tudo.sse.classfilefeatures.extractor

import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

object Application {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    val extractor = new ClassfileFeatureExtractor

    Try(extractor.initialize()) match {
      case Success(_) =>
        val future = extractor.startProcessingLibraries()
        future.onComplete(_ => extractor.shutdown())(extractor.streamMaterializer.executionContext)
      case Failure(ex) =>
        log.error("Failed to initialize classfile feature extractor.", ex)
        extractor.shutdown()
    }
  }

}
