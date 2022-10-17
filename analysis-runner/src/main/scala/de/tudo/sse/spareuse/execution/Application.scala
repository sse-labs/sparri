package de.tudo.sse.spareuse.execution

import com.typesafe.config.Config
import de.tudo.sse.spareuse.core.utils.streaming.{AsyncStreamWorker, StreamingApp}

import scala.util.Try

object Application extends StreamingApp[AnalysisRunnerConfig] {

  private var exitOnEnter: Boolean = true

  override protected def buildWorker(config: AnalysisRunnerConfig): AsyncStreamWorker[_] = {
    exitOnEnter = config.exitOnEnter
    new AnalysisRunner(config)
  }

  override protected def buildConfig(typesafeConfig: Config): Try[AnalysisRunnerConfig] =
    AnalysisRunnerConfig.fromConfig(typesafeConfig)

  override def exitOnReturn: Boolean = exitOnEnter

  override def onComplete(): Unit = log.info("Finished runner execution.")
}
