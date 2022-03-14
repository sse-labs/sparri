package de.tudo.sse.classfilefeatures.common.download

import java.io.{ByteArrayInputStream, InputStream}

import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.HttpClients

import scala.language.postfixOps
import scala.util.Try

class HttpDownloader {

  private val client = HttpClients.createDefault()

  def downloadFromUri(requestedUri: String): Try[InputStream] = {

    val getRequest: HttpGet = new HttpGet(requestedUri)

    var closeableResponse: CloseableHttpResponse = null

    val result = Try {
      closeableResponse = client.execute(getRequest)

      val entityInputStream = closeableResponse.getEntity.getContent
      val entityBytes = Stream.continually(entityInputStream.read).takeWhile(_ != -1).map(_.toByte).toArray

      new ByteArrayInputStream(entityBytes)
    }

    if(closeableResponse != null) closeableResponse.close()

    result
  }

  def shutdown(): Unit = {
    client.close()
  }

}

