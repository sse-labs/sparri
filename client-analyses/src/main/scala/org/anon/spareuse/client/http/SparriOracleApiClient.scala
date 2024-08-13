package org.anon.spareuse.client.http

import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import org.anon.spareuse.core.model.entities.JavaEntities.JavaInvocationType
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.ApplicationMethod
import org.anon.spareuse.webapi.model.oracle.{ApplicationMethodRepr, InitializeResolutionRequest, InvokeStmtRepr, MethodIdentifierRepr, OracleJsonSupport, StartResolutionRequest, TypeNodeRepr}
import org.apache.http.client.HttpResponseException
import org.opalj.br.instructions.{INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, NEW}
import org.opalj.br.{DefinedMethod, Method}
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
        log.info(s"Successfully started resolution for entry point ${apiMethod.ident.declType}.${opalMethod.name}")
        Success(())
    }

  }



  def getToken: Option[String] = sessionToken

  private def opalToApiModel(opalMethod: Method): ApplicationMethodRepr = {
    val ident = MethodIdentifierRepr(opalMethod.classFile.fqn, opalMethod.name, opalMethod.descriptor.toJVMDescriptor)
    val types = opalMethod.body.map(c => c.instructions.filter(_.isInstanceOf[NEW]).map(_.asNEW.objectType.fqn).toList).getOrElse(List.empty)
    val invokes = opalMethod.body.map(c => c
      .instructions
      .zipWithIndex
      .filter{ t => t._1 != null && t._1.isInvocationInstruction}
      .flatMap{

      case (virt: INVOKEVIRTUAL, pc: Int) =>
          val i = MethodIdentifierRepr(virt.declaringClass.toJVMTypeName, virt.name, virt.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Virtual.id, pc))
      case (static: INVOKESTATIC, pc: Int) =>
          val i = MethodIdentifierRepr(static.declaringClass.fqn, static.name, static.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Static.id, pc))
      case (iinvoke: INVOKEINTERFACE, pc: Int) =>
          val i = MethodIdentifierRepr(iinvoke.declaringClass.fqn, iinvoke.name, iinvoke.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Interface.id, pc))
      case (special: INVOKESPECIAL, pc: Int) =>
          val i = MethodIdentifierRepr(special.declaringClass.fqn, special.name, special.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Special.id, pc))
      case (dynamic: INVOKEDYNAMIC, pc: Int) =>
          val i = MethodIdentifierRepr("<unknown-dynamic>", dynamic.name, dynamic.methodDescriptor.toJVMDescriptor)
          Some(InvokeStmtRepr(i, JavaInvocationType.Dynamic.id, pc))
      case _ =>
          None

    }.toSeq).getOrElse(Seq.empty)
    ApplicationMethodRepr(ident, opalMethod.isStatic, types, invokes)
  }

}
