package de.tudo.sse.spareuse.eval

package object performance {

  class TimedResult[T](content: T, durationMs: Long) {
    def getDurationMillis: Long = durationMs
    def getContent: T = content
  }

  def timedExec[T](op: () => T): TimedResult[T] = {
    val start = System.currentTimeMillis()

    val result = op()

    val duration = System.currentTimeMillis() - start

    new TimedResult(result, duration)
  }

}
