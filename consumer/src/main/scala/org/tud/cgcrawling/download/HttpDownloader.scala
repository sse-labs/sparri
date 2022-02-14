// Copyright (C) 2018 The Delphi Team.
// See the LICENCE file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.tud.cgcrawling.download

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

