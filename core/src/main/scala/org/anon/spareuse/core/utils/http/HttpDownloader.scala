package org.anon.spareuse.core.utils.http

import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.HttpClients

import java.io.{ByteArrayInputStream, InputStream}
import scala.language.postfixOps
import scala.util.Try

class HttpDownloader {

  private val client = HttpClients.createDefault()

  def downloadFromUri(requestedUri: String): Try[InputStream] = {

    val getRequest: HttpGet = new HttpGet(requestedUri)

    var closeableResponse: CloseableHttpResponse = null

    val result = Try {
      closeableResponse = client.execute(getRequest)

      if(closeableResponse.getStatusLine.getStatusCode != 200)
        throw  HttpDownloadException(closeableResponse.getStatusLine.getStatusCode, requestedUri, s"Non-Success status while attempting to download.")

      val entityInputStream = closeableResponse.getEntity.getContent
      val entityBytes = LazyList.continually(entityInputStream.read).takeWhile(_ != -1).map(_.toByte).toArray

      new ByteArrayInputStream(entityBytes)
    }

    if(closeableResponse != null) closeableResponse.close()

    result
  }

  def shutdown(): Unit = {
    client.close()
  }

}

case class HttpDownloadException(status: Int, url: String, msg: String) extends Throwable {
  override def getMessage: String = s"Got status $status while downloading $url, message: $msg"
}

