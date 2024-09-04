package org.anon.spareuse.client.http

import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import org.anon.spareuse.core.model.entities.JavaEntities.JavaInvocationType
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.LookupResponseRepresentation
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.ApplicationMethod
import org.anon.spareuse.webapi.model.oracle.{ApplicationMethodRepr, InitializeResolutionRequest, InvokeStmtRepr, LookupResponse, MethodIdentifierRepr, OracleJsonSupport, PullLookupRequestsResponse, StartResolutionRequest, TypeNodeRepr}
import org.apache.http.client.HttpResponseException
import org.opalj.br.instructions.{INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, NEW}
import org.opalj.br.{Code, DefinedMethod, Method}
import spray.json.{JsObject, JsString, enrichAny, enrichString, jsonReader}

import scala.util.{Failure, Success, Try}

class SparriOracleApiClient extends SparriApiClient with OracleJsonSupport {

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

  def startResolutionAt(opalMethod: Method, pc: Int, typesInitialized: Set[String]): Try[Unit] = {
    val apiMethod = opalToApiModel(opalMethod)
    val request = StartResolutionRequest(apiMethod, pc, typesInitialized)

    Try(postJsonAndReturnString("/api/oracle/resolve-entry",
      request.toJson.compactPrint, Map("session-id" -> sessionToken.get))).flatten match {
      case Failure(hrx: HttpResponseException) if hrx.getStatusCode == BadRequest.intValue =>
        if(hrx.getReasonPhrase.contains("session ID")){
          log.error(s"Server failed to recognize our session token")
        } else {
          log.error(s"Request for resolving entry point was invalid: ${hrx.getReasonPhrase}")
        }
        Failure(hrx)

      case Failure(hrx: HttpResponseException) if hrx.getStatusCode == InternalServerError.intValue =>
        log.error(s"Internal server error while requesting resolution of entry point")
        Failure(hrx)

      case Success(_) =>
        Success(())

      case Failure(ex) => Failure(ex)
    }

  }

  def isReadyForInteraction: Boolean = {
    getAsString("/api/oracle/pull-status", rawHeader = Map("session-id" -> sessionToken.get)) match {
      case Success(content) =>
        content.parseJson.convertTo[PullLookupRequestsResponse].isInitialized
      case Failure(ex) =>
        log.error(s"Error checking for interaction readiness", ex)
        false
    }
  }

  def pullStatus(): Try[PullLookupRequestsResponse] = Try {
    getAsString("/api/oracle/pull-status", rawHeader = Map("session-id" -> sessionToken.get)) match {
      case Success(stringResponse) =>
        stringResponse.parseJson.convertTo[PullLookupRequestsResponse]
      case Failure(ex) =>
        throw ex
    }
  }

  def pushResponse(response: LookupResponse): Try[Unit] = Try {
    val json = response.toJson.compactPrint

    val httpResponse = postJsonRaw("/api/oracle/push-update", Some(json), Map("session-id" -> sessionToken.get)).get

    httpResponse.close()

    log.debug(s"Successfully pushed update to oracle")
  }

  def finalizeSession(): Try[Unit] = Try {
    val httpResponse = postJsonRaw("/api/oracle/finalize", None, Map("session-id" -> sessionToken.get)).get

    httpResponse.close()

    log.debug(s"Session finalized: ${sessionToken.get}")

    sessionToken = None
  }



  def getToken: Option[String] = sessionToken

  def opalToApiModel(opalMethod: Method): ApplicationMethodRepr = {
    val ident = MethodIdentifierRepr(opalMethod.classFile.fqn, opalMethod.name, opalMethod.descriptor.toJVMDescriptor)
    val types = opalMethod.body.map(c => c.instructions.filter(_.isInstanceOf[NEW]).map(_.asNEW.objectType.fqn).toList).getOrElse(List.empty)
    val invokes = opalMethod.body.map(getAllInvocationInstructionsAsApiModel).getOrElse(Seq.empty)
    ApplicationMethodRepr(ident, opalMethod.isStatic, types, invokes)
  }

  def getAllInvocationInstructionsAsApiModel(code: Code): List[InvokeStmtRepr] = {
    code
      .instructions
      .zipWithIndex
      .filter { t => t._1 != null && t._1.isInvocationInstruction }
      .flatMap {

        case (virt: INVOKEVIRTUAL, pc: Int) =>
          val i = MethodIdentifierRepr(virt.declaringClass.toJVMTypeName, virt.name, virt.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Virtual.id, pc))
        case (static: INVOKESTATIC, pc: Int) =>
          val i = MethodIdentifierRepr(static.declaringClass.toJVMTypeName, static.name, static.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Static.id, pc))
        case (iinvoke: INVOKEINTERFACE, pc: Int) =>
          val i = MethodIdentifierRepr(iinvoke.declaringClass.toJVMTypeName, iinvoke.name, iinvoke.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Interface.id, pc))
        case (special: INVOKESPECIAL, pc: Int) =>
          val i = MethodIdentifierRepr(special.declaringClass.toJVMTypeName, special.name, special.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Special.id, pc))
        case (dynamic: INVOKEDYNAMIC, pc: Int) =>
          val i = MethodIdentifierRepr("<unknown-dynamic>", dynamic.name, dynamic.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Dynamic.id, pc))
        case _ =>
          None

      }.toList
  }

}
