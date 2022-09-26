package de.tudo.sse.spareuse.mvnpub.maven

import java.io.{BufferedInputStream, InputStream}
import java.net.{HttpURLConnection, URI}

import org.apache.maven.index.reader.ResourceHandler
import scala.concurrent.duration._

class HttpResourceHandler(root : URI) extends ResourceHandler {
  override def locate(name: String): ResourceHandler.Resource = new HttpResource(name)

  override def close(): Unit = {}

  class HttpResource(name: String) extends ResourceHandler.Resource {
    override def read(): InputStream = {

      val target = root.resolve(name).toURL

      val conn: HttpURLConnection = target.openConnection.asInstanceOf[HttpURLConnection]
      conn.setReadTimeout(20.minutes.toMillis.toInt)
      conn.setRequestMethod("GET")
      conn.setRequestProperty("User-Agent", s"Callgraph-Creator-Producer" )

      new BufferedInputStream(conn.getInputStream)
    }
  }

}
