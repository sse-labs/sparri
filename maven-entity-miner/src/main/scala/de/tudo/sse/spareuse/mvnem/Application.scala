package de.tudo.sse.spareuse.mvnem

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.streaming.{AsyncStreamWorker, StreamingApp}

import scala.util.Try

object
Application extends StreamingApp[EntityMinerConfig] {

  private var exitOnEnter: Boolean = true

  override def buildWorker(config: EntityMinerConfig): AsyncStreamWorker[_] = {
    exitOnEnter = config.exitOnEnter
    new MavenEntityMiner(config)
  }

  override def buildConfig(typesafeConfig: Config): Try[EntityMinerConfig] = EntityMinerConfig.fromConfig(typesafeConfig)

  override def onComplete(): Unit = log.info("Finished processing Maven entities from queue.")

  override protected def exitOnReturn: Boolean = exitOnEnter
}
