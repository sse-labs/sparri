package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.{DefinedMethod, MethodIdent}

trait CallGraph {

  protected def lookupMethod(ident: MethodIdent): DefinedMethod

  def reachableMethods(): Set[DefinedMethod]

  def calleesOf(ident: MethodIdent): Iterable[(Int, Set[DefinedMethod])] = calleesOf(lookupMethod(ident))
  def calleesOf(dm: DefinedMethod): Iterable[(Int, Set[DefinedMethod])]

  def calleesOf(ident: MethodIdent, pc: Int): Set[DefinedMethod] = calleesOf(lookupMethod(ident), pc)
  def calleesOf(dm: DefinedMethod, pc: Int): Set[DefinedMethod]

  def callersOf(ident: MethodIdent): Set[DefinedMethod] = callersOf(lookupMethod(ident))
  def callersOf(dm: DefinedMethod): Set[DefinedMethod]

}
