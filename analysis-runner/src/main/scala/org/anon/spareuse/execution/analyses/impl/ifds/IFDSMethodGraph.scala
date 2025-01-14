package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.{FactRep, InternalActivationRep, InternalVariableRep, MethodIFDSRep, StatementRep}
import org.anon.spareuse.execution.analyses.impl.ifds.TaintVariableFacts.{ParameterTaintVariable, TaintFunctionReturn}
import org.opalj.br.{ArrayType, Method, ObjectType}
import org.opalj.tac.{Call, FunctionCall, InstanceFunctionCall}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

class IFDSMethodGraph(methodIdent: MethodIdent) {

  val methodName: String = methodIdent.methodName
  val methodDescriptor: String = methodIdent.methodDescriptor
  val methodDeclaringClassFqn: String = methodIdent.declaredType

  val methodIdentifier: MethodIdent = methodIdent

  lazy val allBasicBlocks: Seq[VirtualStatementNode] = relevantStatementNodes

  lazy val allVariablesReturned: Set[IFDSFact] = statementNodes
    .collect {
      case rvsn: ReturnValueStatementNode if rvsn.variableReturned.isDefined =>
        TaintVariableFacts.buildFact(rvsn.variableReturned.get)
    }
    .toSet

  private val pcToStmtMap: mutable.Map[Int, StatementNode] = new mutable.HashMap[Int, StatementNode]()

  def runWith(initialFacts: Set[IFDSFact])(implicit targetProvider: CallTargetProvider): Set[IFDSFact] = {
    if(statementNodes.isEmpty) initialFacts
    else {
      statementNodes.head.run(initialFacts, methodIdentifier)
    }
  }

  def allFacts: Set[IFDSFact] = pcToStmtMap.values.flatMap(_.allFactsInvolved).toSet ++ Set(IFDSZeroFact)

  def parameterFacts: Set[ParameterTaintVariable] = allFacts.collect{ case x: ParameterTaintVariable => x }

  def isReturnNode(pc: Int): Boolean = pcToStmtMap.contains(pc) && pcToStmtMap(pc).isReturnValue



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



    val node = if(callExpr.isDefined) CallStatementNode(stmt, callExpr.get, callReceiver)
    else StatementNode(stmt)

    if(predecessor.isDefined) node.addPredecessor(predecessor.get)

    pcToStmtMap.put(node.stmtPc, node)
    node
  }

  private[ifds] def putNode(sn: StatementNode): Unit = {
    pcToStmtMap.put(sn.stmtPc, sn)
  }

  def getEntryNode: Option[StatementNode] = {
    val positivePCs = pcToStmtMap.keys.filter(_ >= 0)
    if(positivePCs.isEmpty) None
    else Some(pcToStmtMap(positivePCs.min))
  }

  def getEntryBlock: Option[VirtualStatementNode] = {
    val positivePCBlocks = allBasicBlocks.filter(_.stmtPc >= 0)
    if(positivePCBlocks.isEmpty) None
    else Some(positivePCBlocks.minBy(_.stmtPc))
  }

  def getStatement(pc: Int): Option[StatementNode] = pcToStmtMap.get(pc)

  def hasStatement(pc: Int): Boolean = pcToStmtMap.contains(pc)

  def statementNodes: Seq[StatementNode] = {
    pcToStmtMap.values.toSeq.sortBy(_.stmtPc)
  }

  /**
   * Computes the set of relevant statement nodes only. Nodes are relevant (to IFDS) if they a) have activations, b) are
   * a method call, c) are the method entry, d) are a value return or e) are part of the control flow (if / loop). Nodes
   * are condensed into basic blocks (defined by one relevant node and any number of linear subsequent irrelevant nodes)
   * and the predecessor / successor relation is preserved over those basic blocks.
   *
   * @return A set of virtual statement nodes corresponding to basic blocks - predecessors / successors are set correctly
   */
  def relevantStatementNodes: Seq[VirtualStatementNode] = {
    val entryOpt = getEntryNode

    if(entryOpt.isEmpty)
      return Seq.empty[VirtualStatementNode]

    val visited = mutable.Set.empty[Int]
    val pcToBBLookup = mutable.Map.empty[Int, StatementNode]
    val bbList = mutable.ListBuffer.empty[VirtualStatementNode]
    val workList = mutable.Stack(entryOpt.get)

    // Nodes can be relevant no matter what - based on their activations of statement type
    def isRelevant(node: StatementNode): Boolean = node.isCallNode || node.hasActivations || node.isReturnValue
    // Nodes can be trivial in the context of the graph - if they have one successor, one predecessor and are not relevant
    def isTrivial(node: StatementNode): Boolean =
      node.getSuccessors.size == 1 && node.getPredecessors.size == 1 && !isRelevant(node)

    // Build a basic block starting from the given (relevant) node. Entry nodes to a BB are always relevant and define the
    // BBs PC.
    def buildBasicBlock(entry: StatementNode): VirtualStatementNode = {
      val theNode = new VirtualStatementNode(entry)
      visited.add(entry.stmtPc)
      pcToBBLookup.put(entry.stmtPc, theNode)

      // If the entry to this BB immediately has zero (return) or multiple (if, loop) successors, the block will not contain
      // any further nodes. Push each successor to the worklist and return the BB as it is.
      if(entry.getSuccessors.size != 1){
        entry.getSuccessors.foreach(workList.push)
        return theNode
      }

      // We now have exactly one successor. Start there...
      var current = entry.getSuccessors.head
      // And iterate while the current successor is trivial.
      while(isTrivial(current)){
        // Add the trivial successor to the current BB and update utility structures
        theNode.appendNode(current)
        visited.add(current.stmtPc)
        pcToBBLookup.put(current.stmtPc, theNode)
        current = current.getSuccessors.head
      }

      // We now have that the current successor is no longer trivial. This might be due to two reasons:
      if(isRelevant(current) || current.getPredecessors.size > 1){
        // Means current successor is either loop header / join of an if (if multiple predecessors)
        // or just relevant by definition -> make it a new basic block
        workList.push(current)
      } else {
        // Means current successor is not relevant by itself, but has not exactly one successor, ergo it is the start of
        // an IF or the end of a loop -> make the current node part of the BB
        theNode.appendNode(current)
        visited.add(current.stmtPc)
        pcToBBLookup.put(current.stmtPc, theNode)

        // Start new basic blocks at every successor of the current node
        current.getSuccessors.foreach(workList.push)
      }

      theNode
    }


    // Iterate the list of regular statement nodes (initially only the entry to the method) that need to be processed
    while(workList.nonEmpty){
      val currentNode = workList.pop()

      // If we did not already process this regular node (could be, due to loops) we need to create a new basic block
      if(!visited.contains(currentNode.stmtPc)){
        val basicBlock = buildBasicBlock(currentNode)
        bbList.addOne(basicBlock)
      }

    }

    // Now that all BBs are final, update the predecessor and successor relations so they point to other BBs, not their
    // individual regular nodes
    bbList.foreach{ bb =>
      bb.setPredecessors(bb.entryNode.getPredecessors.map(pred => pcToBBLookup(pred.stmtPc)))
      bb.setSuccessors(bb.getExitNode.getSuccessors.map(succ => pcToBBLookup(succ.stmtPc)))
    }

    // Return the set of all BBs sorted by PC
    bbList.toSeq.sortBy(_.stmtPc)
  }

  def print(): Unit = {
    val facts = this.allFacts.toSeq
    val stmts = this.allBasicBlocks

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
      val pc = s.stmtPc
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

    val stmts = if(squashIdentityStmts) allBasicBlocks else statementNodes

    val stmtReps = stmts
      .map { s =>

        val predecessors = s.getPredecessors.map(_.stmtPc).toList

        val activations = s.allActivations.map{ a =>
          val sourceId = factIdMap(a._1.uniqueIdent)
          val targetIds = a._2.map( f => factIdMap(f.uniqueIdent))
          InternalActivationRep(sourceId, targetIds.toList)
        }.toList

        if(s.isCallNode){
          val csn = s.asCallNode
          val parameterReps = csn.parameterVariables.map{ v =>
            InternalVariableRep(v.variableName, v.defSites.toList)
          }.toList
          val receiverOpt = csn.receiver.map(v => InternalVariableRep(v.variableName, v.defSites.toList))
          StatementRep(csn.stmtPc, false, csn.stmtRep, predecessors, csn.functionName, csn.descriptor, csn.declaringClassFqn, parameterReps, receiverOpt.getOrElse(InternalVariableRep("", List.empty)), InternalVariableRep("", List.empty), activations)
        } else if(s.isReturnValue){
          val rvsn = s.asReturnNode
          val retVar = rvsn.variableReturned.map(r => InternalVariableRep(r.variableName, r.defSites.toList))
          StatementRep(rvsn.stmtPc, true, rvsn.stmtRep, predecessors, "", "", "", List.empty, InternalVariableRep("", List.empty), retVar.getOrElse(InternalVariableRep("", List.empty)), activations)
        } else {
          StatementRep(s.stmtPc, false, s.stmtRep, predecessors, "", "", "", List.empty, InternalVariableRep("", List.empty), InternalVariableRep("", List.empty), activations)
        }
      }
      .toList

    MethodIFDSRep(methodName, methodDeclaringClassFqn, methodDescriptor, stmtReps, factReps)
  }

}

object IFDSMethodGraph {

  def apply(method: Method): IFDSMethodGraph = {
    new IFDSMethodGraph(MethodIdent(method.classFile.thisType.fqn,method.name, method.descriptor.toJVMDescriptor))
  }

  def apply(rep: MethodIFDSRep): IFDSMethodGraph = {
    val theGraph = new IFDSMethodGraph(MethodIdent(rep.declaringClassName, rep.name, rep.descriptor))
    val factDict = rep.facts.map(f => (f.uid, TaintVariableFacts.parseFact(f.identifier))).toMap

    val stmtDict = rep.statements.map{ stmt =>

      if(stmt.isReturn > 0){

        val returnVariableOpt = if(stmt.returnVariable.variableName.isEmpty) None else {
          Some(new LocalVariable(stmt.returnVariable.variableName, stmt.returnVariable.defSites.toSet))
        }

        (stmt.pc, new ReturnValueStatementNode(stmt.pc, stmt.TACRepresentation, returnVariableOpt))
      } else {

        if(stmt.calleeMethodName.nonEmpty){
          val params = stmt.calleeParameterVariables.map(p => new LocalVariable(p.variableName, p.defSites.toSet))
          val receiverOpt = if(stmt.callReceiverVar.variableName.isEmpty) None else Some(new LocalVariable(stmt.callReceiverVar.variableName, stmt.callReceiverVar.defSites.toSet))
          val callNode = new CallStatementNode(stmt.pc, stmt.TACRepresentation, stmt.calleeMethodName, stmt.calleeClassName, stmt.calleeDescriptor, params, receiverOpt)
          (stmt.pc, callNode)
        } else {
          (stmt.pc, new StatementNode(stmt.pc, stmt.TACRepresentation))
        }
      }
    }.toMap

    rep.statements.foreach{ stmtRep =>
      val currNode = stmtDict(stmtRep.pc)
      stmtRep.predecessors.foreach{ predPC =>
        currNode.addPredecessor(stmtDict(predPC))
      }

      theGraph.putNode(currNode)

      stmtRep.activations.foreach{ activation =>
        val factToActivate = factDict(activation.sourceFactId)
        val factsActivating = activation.enablingFactIds.map(factDict).toSet
        currNode.setGeneratesOn(factToActivate, factsActivating)
      }
    }


    theGraph
  }

}

class StatementNode(val stmtPc: Int, val stmtRep: String) {

  private val predecessors: mutable.Set[StatementNode] = new mutable.HashSet
  private val successors: mutable.Set[StatementNode] = new mutable.HashSet

  protected[ifds] val activations: mutable.Map[IFDSFact, mutable.Set[IFDSFact]] = new mutable.HashMap

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

  def run(initialFacts: Set[IFDSFact], currentMethod: MethodIdent)(implicit targetProvider: CallTargetProvider): Set[IFDSFact] = {
    val factsAfter = getFactsAfter(initialFacts)

    if(isReturnValue) factsAfter
    else if(isCallNode){
      val call = asCallNode

      // Compute indexes of callee params that are tainted (according to the current caller context)
      val taintedParameterIndices = call
        .parameterVariables
        .zipWithIndex
        .filter{ case (variable, _) => initialFacts.contains(TaintVariableFacts.buildFact(variable))}
        .map(_._2)

      val targets = targetProvider(currentMethod)(stmtPc)

      if(targets.isEmpty){
        StatementNode.log.warn(s"No targets found for ${currentMethod.toString} at PC $stmtPc")
      }

      var taintReturn = false

      val factsAfterCall = targets.flatMap{ targetGraph =>
        // Select which parameters inside the called methods must be tainted (according to taints in caller)
        val parametersToTaint = targetGraph.parameterFacts.filter(pFact => taintedParameterIndices.contains(pFact.parameterIdx))
        // We only pass non-local facts (i.e. field taints) to callee, as well as params. All other taints are method-specific.
        val factsToPass = initialFacts.filter(f => f == IFDSZeroFact || f.asTaintVariable.isField) ++ parametersToTaint

        // "Run" the target graph and collect all facts valid after invocation (this will include local facts)
        val callResult = targetGraph.statementNodes.head.run(factsToPass, targetGraph.methodIdentifier)

        // Find all variables that may be returned by the callee graph
        val returnVariables = targetGraph.allVariablesReturned

        // Find out if we need to taint the variable that this call is assigned to - that is, if any of the returned variables
        // Is in the set of tainted variables.
        if(callResult.exists( tainted => returnVariables.contains(tainted)))
          taintReturn = true

        // Filter any local facts from callee return - those are not valid inside the caller
        callResult.filter(f => f == IFDSZeroFact || f.asTaintVariable.isField)
      }

      // Combine all facts together as needed. Call-to-Return (factsAfter), Return (factsAfterCall) and also add the
      // artificial function return fact iff we found that the return could be tainted.
      val effectivelyTaintedFacts = if(taintReturn) {
        val retFact = allFactsInvolved
          .find{
            case a: TaintFunctionReturn => a.callPc == call.stmtPc
            case _ => false
          }
          .toSet
          .flatMap{ callReturnFact =>
          getFactsActivatedBy(callReturnFact)
        }

        if(retFact.isEmpty){
          StatementNode.log.warn(s"Could not find return fact although return is tainted")
        }

        factsAfter ++ factsAfterCall ++ retFact
      } else
        factsAfter ++ factsAfterCall

      // Keep on running the current graph with the newly computed set of tainted facts
      getSuccessors.flatMap { successor =>
        successor.run(effectivelyTaintedFacts, currentMethod)
      }
    } else {
      // For normal nodes, just run the successors and combine their results
      getSuccessors.flatMap{ successor =>
        successor.run(factsAfter, currentMethod)
      }
    }
  }

  def getSuccessors: Set[StatementNode] = successors.toSet

  def allFactsInvolved: Set[IFDSFact] = activations.values.flatten.toSet ++ activations.keySet

  def hasActivations: Boolean = activations.nonEmpty

  def isCallNode: Boolean = false

  def asCallNode: CallStatementNode = throw new IllegalStateException("Not a call statement")

  def isReturnValue: Boolean = false

  def asReturnNode: ReturnValueStatementNode = throw new IllegalStateException("Not a return statement")

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

object StatementNode {

  private[ifds] final val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(stmt: TACStmt): StatementNode = {

    if(stmt.isReturnValue){
      val returnedVariableOpt = if(stmt.asReturnValue.expr.isVar) Some(LocalVariable(stmt.asReturnValue.expr.asVar)) else None
      new ReturnValueStatementNode(stmt.pc, stmt.toString, returnedVariableOpt)
    } else new StatementNode(stmt.pc, stmt.toString)
  }

}

class VirtualStatementNode(entry: StatementNode) extends StatementNode(entry.stmtPc, entry.stmtRep) {
  val entryNode: StatementNode = entry

  private[this] var exitNode = entry
  private[this] val innerNodes: mutable.ListBuffer[StatementNode] = mutable.ListBuffer(entry)
  private[this] var predecessors: Set[StatementNode] = Set.empty
  private[this] var successors: Set[StatementNode] = Set.empty

  override def getPredecessors: Set[StatementNode] = predecessors
  override def getSuccessors: Set[StatementNode] = successors

  // The entry node is the only non-trivial node of a basic block. No other node is allowed to have activations
  override protected[ifds] val activations: mutable.Map[IFDSFact, mutable.Set[IFDSFact]] = entryNode.activations

  // The entry node also defines if this basic block is a call - or return node
  override def isCallNode: Boolean = entry.isCallNode

  override def asCallNode: CallStatementNode = entry.asCallNode

  override def isReturnValue: Boolean = entry.isReturnValue

  override def asReturnNode: ReturnValueStatementNode = entry.asReturnNode

  def appendNode(node: StatementNode): Unit = {
    assert(exitNode.getSuccessors.contains(node))

    innerNodes.append(node)

    exitNode = node
  }

  def setPredecessors(preds: Set[StatementNode]): Unit = predecessors = preds
  def setSuccessors(succs: Set[StatementNode]): Unit = successors = succs

  override def addSuccessor(node: StatementNode): Unit = successors = successors ++ Set(node)
  override def addPredecessor(node: StatementNode): Unit = predecessors = predecessors ++ Set(node)

  def getInnerNodes: Seq[StatementNode] = innerNodes.toSeq
  def getExitNode: StatementNode = exitNode
}

class CallStatementNode(stmtPc: Int, stmtRep: String, callMethodName: String, callDeclaringClass: String, callDescriptor: String, callParams: Seq[LocalVariable], callReceiver: Option[LocalVariable]) extends StatementNode(stmtPc, stmtRep){

  val parameterVariables: Seq[LocalVariable] = callParams

  val functionName: String = callMethodName
  val declaringClassFqn: String = callDeclaringClass
  val descriptor: String = callDescriptor
  val receiver: Option[LocalVariable] = callReceiver

  override def isCallNode: Boolean = true

  override def asCallNode: CallStatementNode = this

  override def allFactsInvolved: Set[IFDSFact] = {
    super.allFactsInvolved ++ callParams.map(TaintVariableFacts.buildFact)
  }

}

object CallStatementNode{
  def apply(tacStmt: TACStmt, call: Call[TACVar], callReceiver: Option[TACVar]): CallStatementNode = {
    val callDeclaringClass = call.declaringClass match {
      case ot: ObjectType => ot.fqn
      case at: ArrayType => at.toJava
    }
    val callParams = call.params.filter(_.isVar).map(v => LocalVariable(v.asVar))
    new CallStatementNode(tacStmt.pc, tacStmt.toString, call.name, callDeclaringClass, call.descriptor.toJVMDescriptor, callParams, callReceiver.map(LocalVariable.apply))
  }
}

class ReturnValueStatementNode(stmtPc: Int, stmtRep: String, returnVariable: Option[LocalVariable]) extends StatementNode(stmtPc, stmtRep) {
  val variableReturned: Option[LocalVariable] = returnVariable

  override def isReturnValue: Boolean = true

  override def asReturnNode: ReturnValueStatementNode = this
}

case class LocalVariable(variableName: String, defSites: Set[Int])

object LocalVariable {
  def apply(tacVar: TACVar): LocalVariable = LocalVariable(TaintVariableFacts.normalizeVarName(tacVar), Set.from(tacVar.definedBy.toList))
}






