package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.{FactRep, InternalActivationRep, InternalVariableRep, MethodIFDSRep, StatementRep}
import org.opalj.br.{ArrayType, ArrayTypeSignature, Method, MethodDescriptor, ObjectType}
import org.opalj.tac.{Call, FunctionCall, InstanceFunctionCall, ReturnValue}

import scala.collection.mutable

class IFDSMethodGraph(mName: String, mDeclaringClassFqn: String, mDescriptor: String) {

  val methodName: String = mName
  val methodDescriptor: String = mDescriptor
  val methodDeclaringClassFqn: String = mDeclaringClassFqn

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

  def isReturnNode(pc: Int): Boolean = pcToStmtMap.contains(pc) && pcToStmtMap(pc).stmt.isInstanceOf[ReturnValue[TACVar]]

  def createStatement(stmt: TACStmt, predecessor: Option[StatementNode]): StatementNode = {

    var callExpr: Option[Call[TACVar]] = None
    var callReceiver: Option[TACVar] = None

    if(stmt.isMethodCall) {
      val call = stmt.asMethodCall
      callExpr = Some(call)
      if(call.receiverOption.exists(_.isVar)) callReceiver = call.receiverOption.map(_.asVar)
    } else {
      stmt.forallSubExpressions[TACVar] {
        case fc: FunctionCall[TACVar] if callExpr.isEmpty =>
          callExpr = Some(fc)
          fc match {
            case ifc: InstanceFunctionCall[TACVar] if ifc.receiver.isVar =>
              callReceiver = Some(ifc.receiver.asVar)
            case _ =>
          }
          true
        case _: FunctionCall[TACVar] if callExpr.isDefined =>
          throw new RuntimeException(s"Two function calls in one statement: ${stmt.toString}")
        case _ =>
          false
      }
    }



    val node = if(callExpr.isDefined) new CallStatementNode(stmt, callExpr.get, callReceiver)
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

  def statementNodes: Seq[StatementNode] = {
    pcToStmtMap.values.toSeq.sortBy(_.stmt.pc)
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

  def toResultRepresentation(squashIdentityStmts: Boolean): DefaultIFDSSummaryBuilder.MethodIFDSRep = {

    implicit def boolToInt: Boolean => Int = x => if(x) 1 else 0

    var factId: Int = 0
    val factIdMap = allFacts
      .map { f =>
        val id = factId
        factId += 1
        (f.uniqueIdent, id)
      }
      .toMap

    val factReps = allFacts.map{currFact =>
      val currId = factIdMap(currFact.uniqueIdent)
      FactRep(currId, currFact.uniqueIdent, currFact.displayName)
    }.toList

    val stmts = if(squashIdentityStmts) relevantStatementNodes else statementNodes

    val stmtReps = stmts
      .map { s =>

        val isReturn = isReturnNode(s.stmt.pc)
        val predecessors = s.getPredecessors.map(_.stmt.pc).toList

        val activations = s.allActivations.map{ a =>
          val sourceId = factIdMap(a._1.uniqueIdent)
          val targetIds = a._2.map( f => factIdMap(f.uniqueIdent))
          InternalActivationRep(sourceId, targetIds.toList)
        }.toList

        s match {
          case csn: CallStatementNode =>
            val parameterReps = csn.parameterVariables.map{ v =>
              InternalVariableRep(TaintVariableFacts.normalizeVarName(v), v.definedBy.toList)
            }.toList
            val receiverOpt = csn.receiver.map(v => InternalVariableRep(TaintVariableFacts.normalizeVarName(v), v.definedBy.toList))
            StatementRep(csn.stmt.pc, isReturn, csn.stmt.toString, predecessors, csn.functionName, csn.descriptor.toJVMDescriptor, csn.declaringClassFqn, parameterReps, receiverOpt.getOrElse(InternalVariableRep("", List.empty)), activations)
          case sn: StatementNode =>
            StatementRep(sn.stmt.pc, isReturn, sn.stmt.toString, predecessors, "", "", "", List.empty, InternalVariableRep("", List.empty), activations)
        }

      }
      .toList

    MethodIFDSRep(methodName, methodDeclaringClassFqn, methodDescriptor, stmtReps, factReps)
  }

}

object IFDSMethodGraph {
  def apply(method: Method): IFDSMethodGraph = {
    new IFDSMethodGraph(method.name, method.classFile.thisType.fqn, method.descriptor.toJVMDescriptor)
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

  def getPredecessors: Set[StatementNode] = predecessors.toSet

  def addSuccessor(node: StatementNode): Unit = {
    if (!successors.contains(node)) successors.add(node)
    if (!node.predecessors.contains(this)) node.predecessors.add(this)
  }

  def getSuccessors: Set[StatementNode] = successors.toSet

  def allFactsInvolved: Set[IFDSFact] = activations.values.flatten.toSet ++ activations.keySet

  def hasActivations: Boolean = activations.nonEmpty

  def isCallNode: Boolean = false

  def asCallNode: CallStatementNode = throw new IllegalStateException("Not a call statement")

  def hasActivation(fact: IFDSFact): Boolean = activations.contains(fact)

  def activatesOn(fact: IFDSFact): Set[IFDSFact] = activations.get(fact).map(_.toSet).getOrElse(Set.empty)

  def getFactsKilled: Set[IFDSFact] = activations.filter(t => t._2.isEmpty).keys.toSet

  def getFactsActivatedBy(fact: IFDSFact): Set[IFDSFact] = activations.filter(t => t._2.contains(fact)).keys.toSet

  def getFactsAfter(currentFacts: Set[IFDSFact]): Set[IFDSFact] = {
    (currentFacts.filter(f => !hasActivation(f)) ++ currentFacts.flatMap(f => getFactsActivatedBy(f))).diff(getFactsKilled)
  }


  type Activation = (IFDSFact, Set[IFDSFact])
  def allActivations: Seq[Activation] = activations.toSeq.map(t => (t._1, t._2.toSet))
}

class CallStatementNode(stmt: TACStmt, callExpr: Call[TACVar], callReceiver: Option[TACVar]) extends StatementNode(stmt){

  val parameterVariables: Seq[TACVar] =
    callExpr
      .params
      .filter(_.isVar)
      .map(_.asVar)

  val functionName: String = callExpr.name
  val declaringClassFqn: String = callExpr.declaringClass match {
    case ot: ObjectType => ot.fqn
    case at: ArrayType => at.toJava //TODO: Find out if we need to keep track of operations on arrays
  }
  val descriptor: MethodDescriptor = callExpr.descriptor
  val receiver: Option[TACVar] = callReceiver

  override def isCallNode: Boolean = true

  override def asCallNode: CallStatementNode = this

}






