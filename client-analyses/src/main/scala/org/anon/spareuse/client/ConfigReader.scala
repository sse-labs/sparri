package org.anon.spareuse.client

import com.typesafe.config.ConfigFactory

object ConfigReader {

  private final val prefix = "spar-reuse.client."

  private[client] lazy val baseConfig = ConfigFactory.load()

  def getSparriHost: String = {
    val path = prefix + "sparri-host"
    if(baseConfig.hasPath(path)) baseConfig.getString(path)
    else throw new IllegalArgumentException(s"The given configuration is missing a required member: $path")
  }

  def getSparriPort: Int = {
    val path = prefix + "sparri-port"
    if(baseConfig.hasPath(path)) baseConfig.getInt(path)
    else throw new IllegalArgumentException(s"The given configuration is missing a required member: $path")
  }
}
