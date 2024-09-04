package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.ApplicationMethod
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.MethodIFDSRep
import org.opalj.br.Method
import org.opalj.tac.{AITACode, DUVar, DVar, Stmt, TACMethodParameter, UVar}
import org.opalj.value.ValueInformation

package object ifds {

  type TACVar = DUVar[ValueInformation]
  type TACUVar = UVar[ValueInformation]
  type TACDVar = DVar[ValueInformation]
  type TACStmt = Stmt[TACVar]
  type MethodTACProvider = Method => AITACode[TACMethodParameter, ValueInformation]

  case object IFDSZeroFact extends IFDSFact {
    override def toString: String = "ZERO"

    override def uniqueIdent: String = "<0>"

    override def displayName: String = "<0>"
  }


  case class ApplicationMethodWithSummary(method: ApplicationMethod, ifdsSummary: MethodIFDSRep)
}
