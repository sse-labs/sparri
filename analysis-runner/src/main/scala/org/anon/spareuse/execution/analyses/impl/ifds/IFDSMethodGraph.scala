package org.anon.spareuse.execution.analyses.impl.ifds

import org.opalj.br.{Method, MethodDescriptor}
import org.opalj.tac.{FunctionCall, ReturnValue}

import scala.collection.mutable

class IFDSMethodGraph(val method: Method) {

  private val pcToStmtMap: mutable.Map[Int, StatementNode] = new mutable.HashMap[Int, StatementNode]()

  def allFacts: Set[IFDSFact] = pcToStmtMap.values.flatMap(_.allFactsInvolved).toSet ++ Set(IFDSZeroFact)

  def possibleReturns: Set[TACVar] = pcToStmtMap
    .values
    .map(_.stmt)
    .collect{
      case ReturnValue(_, retVariable: TACVar) =>
        retVariable
      case ReturnValue(_, expr) =>
        throw new RuntimeException(s"Unhandled return ${expr.getClass}")
    }
    .toSet

  def createStatement(stmt: TACStmt, predecessor: Option[StatementNode]): StatementNode = {

    var callExpr: Option[FunctionCall[TACVar]] = None

    stmt.forallSubExpressions[TACVar]{
      case fc: FunctionCall[TACVar] if callExpr.isEmpty=>
        callExpr = Some(fc)
        true
      case _: FunctionCall[TACVar] if callExpr.isDefined =>
        throw new RuntimeException(s"Two function calls in one statement: ${stmt.toString}")
      case _ =>
        false
    }

    val node = if(callExpr.isDefined) new CallStatementNode(stmt, callExpr.get)
    else new StatementNode(stmt)

    if(predecessor.isDefined) node.addPredecessor(predecessor.get)

    pcToStmtMap.put(node.stmt.pc, node)
    node
  }

  def getStatement(pc: Int): Option[StatementNode] = pcToStmtMap.get(pc)

  def hasStatement(pc: Int): Boolean = pcToStmtMap.contains(pc)

  def relevantStatementNodes: Seq[StatementNode] = {
    // Returns only those statements that change any of the activations, or involve a call, or are return statements
    pcToStmtMap
      .values
      .filter(stmtNode => stmtNode.isCallNode || stmtNode.hasActivations || stmtNode.stmt.isReturnValue)
      .toSeq
      .sortBy(_.stmt.pc)
  }

  def print(): Unit = {
    val facts = this.allFacts.toSeq
    val stmts = this.relevantStatementNodes

    val zeroColumnOffset = 5 + 4

    val columnWidth = Array(zeroColumnOffset) ++ facts.map(f => Math.max(15, f.displayName.length + 3)).toArray

    def skip(n: Int, s: String = " "): Unit = Range(0, n).foreach(_ => System.out.print(s))
    def pad(): Unit = System.out.print(" | ")

    // HEADER ROW
    skip(zeroColumnOffset)
    facts.zipWithIndex.foreach{ case (f, i) =>
      pad()
      val header = s"$i: " + f.displayName
      val toSkip = (columnWidth(i + 1) - header.length) / 2
      skip(toSkip)
      System.out.print(header)
      skip(toSkip)
      if(2*toSkip + header.length < columnWidth(i + 1)) skip(1)
    }
    println()

    skip(columnWidth.sum + (columnWidth.length - 1) * 3, "-")
    println()

    stmts.foreach{ s =>
      val pc = s.stmt.pc
      val pcStr = if(pc < 10) s"   $pc"
      else if(pc < 100) s"  $pc"
      else if(pc < 1000) s" $pc"
      else pc.toString
      System.out.print(s"[pc=$pcStr]")
      facts.zipWithIndex.foreach{ case (f, i) =>
        pad()
        val factStr = if(s.hasActivation(f)) "{" + s.activatesOn(f).map(af => facts.indexOf(af)).mkString(",") + "}" else "{" + facts.indexOf(f)  +"}"
        val toSkip = (columnWidth(i + 1) - factStr.length) / 2
        skip(toSkip)
        System.out.print(factStr)
        skip(toSkip)
        if(2*toSkip + factStr.length < columnWidth(i + 1)) skip(1)
      }
      println()

    }


  }

}

class StatementNode(val stmt: TACStmt) {
  private val predecessors: mutable.Set[StatementNode] = new mutable.HashSet
  private val successors: mutable.Set[StatementNode] = new mutable.HashSet

  private val activations: mutable.Map[IFDSFact, mutable.Set[IFDSFact]] = new mutable.HashMap

  def setKillsFact(fact: IFDSFact): Unit = {
    assert(fact != IFDSZeroFact)
    setGeneratesOn(fact, Set.empty)
  }

  def setGeneratesFact(fact: IFDSFact): Unit = {
    assert(fact != IFDSZeroFact)
    setGeneratesOn(fact, Set(IFDSZeroFact))
  }

  def setGeneratesOn(factToGenerate: IFDSFact, factsEnabling: Set[IFDSFact]): Unit = {
    assert(factToGenerate != IFDSZeroFact)
    if (!activations.contains(factToGenerate)) {
      activations.put(factToGenerate, mutable.Set.from(factsEnabling))
    } else {
      activations(factToGenerate).addAll(factsEnabling)
    }
  }

  def addPredecessor(node: StatementNode): Unit = {
    if (!predecessors.contains(node)) predecessors.add(node)
    if (!node.successors.contains(this)) node.successors.add(this)
  }

  def addSuccessor(node: StatementNode): Unit = {
    if (!successors.contains(node)) successors.add(node)
    if (!node.predecessors.contains(this)) node.predecessors.add(this)
  }

  def allFactsInvolved: Set[IFDSFact] = activations.values.flatten.toSet ++ activations.keySet

  def hasActivations: Boolean = activations.nonEmpty

  def isCallNode: Boolean = false

  def asCallNode: CallStatementNode = throw new IllegalStateException("Not a call statement")

  def hasActivation(fact: IFDSFact): Boolean = activations.contains(fact)

  def activatesOn(fact: IFDSFact): Set[IFDSFact] = activations.get(fact).map(_.toSet).getOrElse(Set.empty)
}

class CallStatementNode(stmt: TACStmt, callExpr: FunctionCall[TACVar]) extends StatementNode(stmt){

  val parameterVariables: Seq[TACVar] =
    callExpr
      .params
      .filter(_.isVar)
      .map(_.asVar)

  val functionName: String = callExpr.name
  val declaringClassJVMName: String = callExpr.declaringClass.toJVMTypeName
  val descriptor: MethodDescriptor = callExpr.descriptor

  override def isCallNode: Boolean = true

  override def asCallNode: CallStatementNode = this

}






