package org.anon.spareuse.webapi.model

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaInvocationType, JavaInvokeStatement}
import org.anon.spareuse.execution.analyses.impl.cg.AbstractRTABuilder.TypeNode
import org.anon.spareuse.execution.analyses.impl.cg.InteractiveOracleAccessor.LookupResponseRepresentation
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.{ApplicationMethod, MethodIdent}
import org.anon.spareuse.execution.analyses.impl.ifds.ApplicationMethodWithSummary

package object oracle {

  def toModel(invokeRepr: InvokeStmtRepr): JavaInvokeStatement = new JavaInvokeStatement(invokeRepr.declIdent.mName,
    invokeRepr.declIdent.declType, invokeRepr.declIdent.mDescr, JavaInvocationType.fromId(invokeRepr.invokeType), invokeRepr.pc,
    -1, "<LOCAL>")

  def toModel(methodIdentRepr: MethodIdentifierRepr): MethodIdent =
    MethodIdent(methodIdentRepr.declType, methodIdentRepr.mName, methodIdentRepr.mDescr)

  def toModel(appMethodRepr: ApplicationMethodRepr): ApplicationMethod = {
    new ApplicationMethod(toModel(appMethodRepr.ident), appMethodRepr.isStatic, appMethodRepr.types, appMethodRepr.invokes.map(toModel))
  }

  def toModel(nodeRepr: TypeNodeRepr): TypeNode = new TypeNode(nodeRepr.fqn, nodeRepr.superFqn, nodeRepr.interfaceFqns, nodeRepr.isInterface)

  def toModel(response: LookupResponse): LookupResponseRepresentation = LookupResponseRepresentation(
    response.requestId,
    response.targets.map(methodWithSummary => ApplicationMethodWithSummary(toModel(methodWithSummary.methodRep), methodWithSummary.summaryRepr)),
    response.noDefs,
    response.hasFatalErrors)

}
