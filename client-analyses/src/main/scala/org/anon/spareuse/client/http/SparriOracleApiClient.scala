package org.anon.spareuse.client.http

import org.anon.spareuse.webapi.model.oracle.{InitializeResolutionRequest, OracleJsonSupport, TypeNodeRepr}
import spray.json.{JsObject, JsString, enrichAny, enrichString, jsonReader}

import scala.util.{Failure, Success, Try}

class SparriOracleApiClient extends SparriApiClient with OracleJsonSupport{

  private[http] var sessionToken: Option[String] = None


  def startOracleSession(dependencyNames: Set[String],
                         projectTypes: Set[TypeNodeRepr],
                         initializedTypes: Set[String],
                         resolutionModeId: Int,
                         jreVersion: Option[String] = None): Try[Unit] = {

    val entity = InitializeResolutionRequest(dependencyNames, projectTypes, initializedTypes, jreVersion, resolutionModeId)

    Try(postJsonAndReturnString("/api/oracle/start-session", entity.toJson.compactPrint)).flatten.flatMap{ stringResponse =>
      Try {
        val jsObj = stringResponse.parseJson.asJsObject()
        if(!jsObj.fields.contains("session-id") || !jsObj.fields("session-id").isInstanceOf[JsString])
          throw new IllegalStateException(s"Server response did not contain a session token")

        val sessionId = jsObj.fields("session-id").asInstanceOf[JsString].value

        if(sessionId.isBlank)
          throw new IllegalStateException(s"Server response did contain a malformed (blank) session token")

        this.sessionToken = Some(sessionId)
      }
    }

  }

  def getToken: Option[String] = sessionToken

}
