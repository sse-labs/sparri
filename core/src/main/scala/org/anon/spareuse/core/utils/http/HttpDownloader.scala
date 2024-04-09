package org.anon.spareuse.core.utils.http

import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.HttpClients

import java.io.{ByteArrayInputStream, InputStream}
import scala.language.postfixOps
import scala.util.Try

class HttpDownloader {

  case class StreamWithHeaders(contentStream: InputStream, headers: Map[String, String])

  private val client = HttpClients.createDefault()

  def downloadFromUri(requestedUri: String, headersToRead: List[String] = List.empty): Try[StreamWithHeaders] = {

    val getRequest: HttpGet = new HttpGet(requestedUri)

    var closeableResponse: CloseableHttpResponse = null

    val result = Try {
      closeableResponse = client.execute(getRequest)

      if(closeableResponse.getStatusLine.getStatusCode != 200)
        throw  HttpDownloadException(closeableResponse.getStatusLine.getStatusCode, requestedUri, s"Non-Success status while attempting to download.")

      val entityInputStream = closeableResponse.getEntity.getContent

      val responseHeaderMap = headersToRead.flatMap { headerName =>
        closeableResponse.getAllHeaders.find( h => h.getName == headerName).map( h => (headerName, h.getValue))
      }.toMap

      val entityBytes = LazyList.continually(entityInputStream.read).takeWhile(_ != -1).map(_.toByte).toArray

      StreamWithHeaders(new ByteArrayInputStream(entityBytes), responseHeaderMap)
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

