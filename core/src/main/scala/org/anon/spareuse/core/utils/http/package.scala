package org.anon.spareuse.core.utils

import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}

import scala.util.Try

package object http {

  def checkUriExists(uri: String, client: CloseableHttpClient): Boolean = {
    var closeableResp: CloseableHttpResponse = null

    val result = Try {
      closeableResp = client.execute(new HttpGet(uri))

      closeableResp.getStatusLine.getStatusCode == 200
    }

    if(closeableResp != null) closeableResp.close()

    result.getOrElse(false)
  }

  def checkUriExists(uri: String): Boolean = {
    val client = HttpClients.createDefault()

    val result = checkUriExists(uri, client)

    client.close()

    result
  }

}
