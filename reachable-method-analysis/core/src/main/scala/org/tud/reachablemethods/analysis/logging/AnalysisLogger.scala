package org.tud.reachablemethods.analysis.logging

import org.slf4j.{Logger, LoggerFactory}

class AnalysisLogger {

  def debug[T](msg: String)(implicit c: Class[T]): Unit = withLogger(c, l => l.debug(msg))

  def info[T](msg: String)(implicit c: Class[T]): Unit = withLogger(c, l => l.info(msg))

  def warn[T](msg: String)(implicit c: Class[T]): Unit = withLogger(c, l => l.warn(msg))

  def error[T](msg: String)(implicit c: Class[T]): Unit = withLogger(c, l => l.error(msg))

  def error[T](msg: String, x: Throwable)(implicit c: Class[T]): Unit = withLogger(c, l => l.error(msg, x))

  protected def withLogger[T](implicit c: Class[T], func: Logger => Unit): Unit = {
    func(LoggerFactory.getLogger(c))
  }

}
