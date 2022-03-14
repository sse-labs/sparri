package de.tudo.sse.classfilefeatures.common.opal

import org.opalj.br.instructions.{GETFIELD, GETSTATIC, Instruction, PUTFIELD, PUTSTATIC}

object OPALUtilities {

  def isFieldAccessInstruction(i: AnyRef): Boolean = i match {
    case GETFIELD | PUTFIELD | GETSTATIC | PUTSTATIC => true
    case _ => false
  }

  def isInvocationInstruction(i: Instruction): Boolean = i != null && i.isInvocationInstruction

}
