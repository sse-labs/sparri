package org.anon.spareuse.core.utils

import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}

trait EnhancedLogging {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  private var opIdCnt: Int = 0

  def timedOp[T](opDescription: String, op:() => Try[T]): Try[T] = {
    val opId = s"[O-$opIdCnt]"
    opIdCnt += 1

    log.debug(s"$opId Begin: $opDescription")
    val start = System.nanoTime()

    val result = op.apply()

    val time = System.nanoTime() - start

    val timeString = if (time < 5000) s"${time}ns"
    else if (time < 5000000) s"${time / 1000}ms"
    else s"${time / 1000000}s"

    if(result.isSuccess)
      log.debug(s"$opId finished successfully in $timeString")
    else
      log.debug(s"$opId failed in $timeString")

    result
  }
}
