package org.tud.cgcrawling.utils

object StreamingSignals {
  case object Ack

  case object StreamInitialized
  case object StreamCompleted
  final case class StreamFailure(ex: Throwable)
}
