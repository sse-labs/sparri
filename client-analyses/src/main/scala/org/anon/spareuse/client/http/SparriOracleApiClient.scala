package org.anon.spareuse.client.http

import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import org.anon.spareuse.core.model.entities.JavaEntities.JavaInvocationType
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.{FactRep, MethodIFDSRep}
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSFact, TaintVariableFacts}
import org.anon.spareuse.webapi.model.oracle.{ApplicationMethodRepr, ApplicationMethodWithSummaryRepr, IFDSQueryRequest, InitializeResolutionRequest, InvokeStmtRepr, LookupResponse, MethodIdentifierRepr, OracleJsonSupport, PullLookupRequestsResponse, StartResolutionRequest, TypeNodeRepr}
import org.opalj.br.instructions.{INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, NEW}
import org.opalj.br.{Code, Method}
import spray.json.{JsString, enrichAny, enrichString}

import scala.concurrent.duration.DurationInt
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

  def startResolutionAt(opalMethod: Method, methodIFDSRep: MethodIFDSRep, pc: Int, typesInitialized: Set[String]): Try[Unit] = {
    val apiMethod = opalToApiModel(opalMethod)
    val request = StartResolutionRequest(ApplicationMethodWithSummaryRepr(apiMethod, methodIFDSRep), pc, typesInitialized)

    Try(postJsonAndReturnString("/api/oracle/resolve-entry",
      request.toJson.compactPrint, Map("session-id" -> sessionToken.get))).flatten match {
      case Failure(hrx: HttpResponseException) if hrx.code == BadRequest.intValue =>
        if(hrx.msg.contains("session ID")){
          log.error(s"Server failed to recognize our session token")
        } else {
          log.error(s"Request for resolving entry point was invalid: ${hrx.msg}")
        }
        Failure(hrx)

      case Failure(hrx: HttpResponseException) if hrx.code == InternalServerError.intValue =>
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

  def closeSession(): Try[Unit] = Try {
    postJsonRaw("/api/oracle/close", None, Map("session-id" -> sessionToken.get)).get

    log.debug("Successfully closed session")
  }

  def pushResponse(response: LookupResponse): Try[Unit] = Try {
    val json = response.toJson.compactPrint

    postJsonRaw("/api/oracle/push-update", Some(json), Map("session-id" -> sessionToken.get)).get

    log.debug(s"Successfully pushed update to oracle")
  }

  def finalizeSession(): Try[Unit] = Try {
    postJsonRaw("/api/oracle/finalize", None, Map("session-id" -> sessionToken.get), timeout = 60.seconds).get

    log.debug(s"Session finalized: ${sessionToken.get}")
  }

  def doQuery(methodToQuery: MethodIdent, facts: Set[IFDSFact]): Try[Set[IFDSFact]] = {
    val factRepresentations = facts.map(f => FactRep(-1, f.uniqueIdent, f.displayName))
    val methodIdentRep = opalToApiModel(methodToQuery)

    val request = IFDSQueryRequest(methodIdentRep, factRepresentations).toJson.compactPrint

    Try {
      val responseString = postJsonAndReturnString("/api/oracle/query",
        request,
        rawHeader = Map("session-id" -> sessionToken.get),
        timeout = 60.seconds).get

      responseString
        .parseJson
        .convertTo[Set[FactRep]]
        .map(factRep => TaintVariableFacts.parseFact(factRep.identifier))
    }
  }


  def getToken: Option[String] = sessionToken

  def opalToApiModel(methodIdent: MethodIdent): MethodIdentifierRepr = {
    MethodIdentifierRepr(methodIdent.declaredType, methodIdent.methodName, methodIdent.methodDescriptor)
  }

  def opalToApiModel(opalMethod: Method): ApplicationMethodRepr = {
    val ident = MethodIdentifierRepr(opalMethod.classFile.fqn, opalMethod.name, opalMethod.descriptor.toJVMDescriptor)
    val types = opalMethod.body.map(c => c.instructions.filter(_.isInstanceOf[NEW]).map(_.asNEW.objectType.fqn).toList).getOrElse(List.empty)
    val invokes = opalMethod.body.map(getAllInvocationInstructionsAsApiModel).getOrElse(Seq.empty)
    ApplicationMethodRepr(ident, opalMethod.isStatic, types, invokes)
  }

  private def getAllInvocationInstructionsAsApiModel(code: Code): List[InvokeStmtRepr] = {
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
