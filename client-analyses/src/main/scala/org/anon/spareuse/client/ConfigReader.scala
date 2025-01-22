package org.anon.spareuse.client

import com.typesafe.config.ConfigFactory
import org.anon.spareuse.core.utils.tryGetEnvOrElse

object ConfigReader {

  private final val prefix = "spar-reuse.client."

  private[client] lazy val baseConfig = ConfigFactory.load()

  def getMavenHome: String = {
    val homeProp = System.getProperty("SPARRI_MVN_HOME")
    if(homeProp != null) homeProp
    else {
      val homeEnv = System.getenv("maven.home")
      if(homeEnv != null) homeEnv
      else {
        val path = prefix + "mvn-home"
        if(baseConfig.hasPath(path)) baseConfig.getString(path)
        else throw new IllegalArgumentException(s"Maven home not set. Use System property (SPARRI_MVN_HOME), environment variable (maven.home) or config ($path) to set Maven home directory")
      }
    }
  }

  def getSparriHost: String = {
    val hostProp = System.getProperty("SPARRI_API_HOST")
    if(hostProp != null) hostProp
    else {
      val path = prefix + "sparri-host"
      if (baseConfig.hasPath(path)) baseConfig.getString(path)
      else throw new IllegalArgumentException(s"The given configuration is missing a required member: $path")
    }
  }

  def getSparriPort: Int = {
    val portProp = System.getProperty("SPARRI_API_PORT")
    if(portProp != null){
      val intRes = portProp.toIntOption
      if(intRes.isDefined) intRes.get
      else throw new IllegalStateException(s"Invalid SPARRI API port specified via environment: Must be integer")
    } else {
      val path = prefix + "sparri-port"
      if (baseConfig.hasPath(path)) baseConfig.getInt(path)
      else throw new IllegalArgumentException(s"The given configuration is missing a required member: $path")
    }

  }
}
