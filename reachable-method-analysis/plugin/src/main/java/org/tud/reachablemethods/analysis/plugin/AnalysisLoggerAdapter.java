package org.tud.reachablemethods.analysis.plugin;

import org.apache.maven.plugin.logging.Log;
import org.tud.reachablemethods.analysis.logging.AnalysisLogger;

public class AnalysisLoggerAdapter extends AnalysisLogger {

    private final Log log;
    private boolean debugEnabled;

    public AnalysisLoggerAdapter(Log pLog){
        this.log = pLog;
        this.debugEnabled = false;
    }

    public void enableDebug(boolean pEnabled){
        this.debugEnabled = pEnabled;
    }

    @Override
    public <T> void debug(String msg, Class<T> c) {
        if(debugEnabled) log.debug(msg);
    }

    @Override
    public <T> void info(String msg, Class<T> c) {
        log.info(msg);
    }

    @Override
    public <T> void warn(String msg, Class<T> c) {
        log.warn(msg);
    }

    @Override
    public <T> void error(String msg, Class<T> c) {
        log.error(msg);
    }

    @Override
    public <T> void error(String msg, Throwable x, Class<T> c) {
        log.error(msg, x);
    }
}
