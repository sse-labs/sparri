package de.tudo.sse.spareuse.mvnpub

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.streaming.{AsyncStreamWorker, StreamingApp}

import scala.util.Try

object Application extends StreamingApp[MavenEntityPublisherConfig] {

  override protected def buildConfig(typesafeConfig: Config): Try[MavenEntityPublisherConfig] =
    MavenEntityPublisherConfig.fromConfig(typesafeConfig)

  override protected def buildWorker(config: MavenEntityPublisherConfig): AsyncStreamWorker[_] =
    new MavenEntityNamePublisher(config)

  override protected def onComplete(): Unit = log.info("Finished publishing Maven entity names.")

}
