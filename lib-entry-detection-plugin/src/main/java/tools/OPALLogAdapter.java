package tools;

import org.apache.maven.plugin.logging.Log;
import org.opalj.log.GlobalLogContext$;
import org.opalj.log.LogContext;
import org.opalj.log.LogMessage;
import org.opalj.log.OPALLogger;

public class OPALLogAdapter implements OPALLogger {

    private final Log internalLog;

    private boolean infoLevelEnabled = true;

    private OPALLogAdapter(Log log){
        internalLog = log;
    }

    public static void updateGlobalContext(OPALLogger logger) {
        OPALLogger.updateLogger(GlobalLogContext$.MODULE$, logger);
    }

    public static OPALLogAdapter buildPluginLogAdapter(Log log) {
        return new OPALLogAdapter(log);
    }

    public static OPALLogger buildNullLogger() {
        return new OPALLogger() {
            @Override
            public void log(LogMessage message, LogContext ctx) {

            }

            @Override
            public void logOnce(LogMessage message, LogContext ctx) {
                OPALLogger.super.logOnce(message, ctx);
            }
        };
    }

    @Override
    public void log(LogMessage message, LogContext ctx) {
        if(internalLog != null) {
            if(message.level().value() == org.opalj.log.Error.value()){
                internalLog.error(message.message());
            } else if (message.level().value() == org.opalj.log.Warn.value()) {
                internalLog.warn(message.message());
            } else if (infoLevelEnabled && message.level().value() == org.opalj.log.Info.value()) {
                internalLog.info(message.message());
            }
        } else {
            System.err.println("No log instance configured for OPAL log adapter");
        }
    }

    public void setInfoLevelEnabled(boolean enabled){
        infoLevelEnabled = enabled;
    }

}
