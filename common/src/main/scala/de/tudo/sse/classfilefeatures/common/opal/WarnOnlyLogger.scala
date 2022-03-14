package de.tudo.sse.classfilefeatures.common.opal

import org.opalj.log.{LogContext, LogMessage, OPALLogger}
import org.slf4j.{Logger, LoggerFactory}

/**
 * Default OPAL logger used when no custom logger is specified. Only prints warnings and errors to the internal logger.
 * @param ct ClassTag of the actual class using this Logger instance.
 */
class WarnOnlyLogger(ct: Class[_]) extends OPALLogger {
  private val internalLog: Logger =  LoggerFactory.getLogger(ct)

  private val exclusionPrefixes = Set("java/lang/ClassLoader does not define")

  override def log(message: LogMessage)(implicit ctx: LogContext): Unit = {
    if(!exclusionPrefixes.exists(p => message.message.startsWith(p))){
      message.level match {
        case org.opalj.log.Error =>
          internalLog.error(message.message)
        case org.opalj.log.Warn =>
          internalLog.warn(message.message)
        case _ =>
      }
    }
  }
}

