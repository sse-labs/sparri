package org.anon.spareuse.execution.analyses.impl.ifds.reachability

import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.MethodIdent
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.MethodIFDSRep
import org.anon.spareuse.execution.analyses.impl.ifds.TaintVariableFacts.{ParameterTaintVariable, TaintFunctionReturn}
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSFact, IFDSMethodGraph, IFDSZeroFact, StatementNode, TaintVariableFacts}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class IFDSMethodRunner private(private[ifds] val environment: IFDSRunnerEnvironment){

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def hasMethod(methodIdent: MethodIdent): Boolean = environment.hasSummary(methodIdent)

  def resolveFrom(methodIdent: MethodIdent, initialFacts: Set[IFDSFact]): Set[IFDSFact] = {

    // Report soft error if no summary is present
    if(!environment.hasSummary(methodIdent)){
      log.warn(s"Missing summary for method ${methodIdent.toString}")
      return initialFacts
    }

    val methodIFDSGraph = environment.getSummary(methodIdent)

    // Methods without statements will not affect the set of facts
    if(methodIFDSGraph.allBasicBlocks.isEmpty || methodIFDSGraph.getEntryBlock.isEmpty){
      return initialFacts
    }

    // We operate exclusively on basic blocks. Compute which activations (facts) we did not yet see (*new*) for the entry to this method
    val firstBasicBlock = methodIFDSGraph.getEntryBlock.get
    val newActivations = environment.newActivationsAt(methodIdent, firstBasicBlock.stmtPc, initialFacts)

    // If we already processed this method at least once and have seen all current facts before: Return the result for
    // this method immediately
    if(newActivations.isEmpty){
      return environment.methodResultingFacts(methodIdent)
    }

    // This now means we have new activations or we are doing the initial pass over the method with empty facts
    case class ResolverTask(method: MethodIdent, statementNode: StatementNode, activations: Set[IFDSFact])

    log.info(s"Starting to resolve IFDS query. Entry=${methodIdent.toString} PC=${firstBasicBlock.stmtPc} Facts=${newActivations.map(_.displayName).mkString}")

    val worklist = mutable.Queue(ResolverTask(methodIdent, firstBasicBlock, newActivations))

    var cnt = 0L

    def writeProgress(): Unit = {
      log.info(s"Worklist-Size: ${worklist.size}")
    }

    while(worklist.nonEmpty){
      val currentTask = worklist.dequeue()
      val currentNode = currentTask.statementNode

      val currentRelevantActivations = environment.newActivationsAt(currentTask.method, currentNode.stmtPc, currentTask.activations)

      if(currentRelevantActivations.nonEmpty){
        cnt += 1

        if(cnt % 1000 == 0) writeProgress()

        // Calling this early will effectively deal with recursion issues
        environment.setVisitedWith(currentTask.method, currentNode.stmtPc, currentRelevantActivations)

        var factsAfter = currentNode.getFactsAfter(currentRelevantActivations)

        if(currentNode.isCallNode){
          val theCall = currentNode.asCallNode

          // Compute indexes of callee params that are tainted (according to the current caller context)
          val taintedParameterIndices = theCall
            .parameterVariables
            .zipWithIndex
            .filter{ case (variable, _) => currentRelevantActivations.contains(TaintVariableFacts.buildFact(variable))}
            .map(_._2)

          environment.getTargetMethodSummaries(theCall, currentTask.method).foreach{ targetMethod =>

            targetMethod.entryBlock match {
              case Some(targetEntryBlock) =>
                // Select which parameters inside the called methods must be tainted (according to taints in caller)
                val parametersToTaint = taintedParameterIndices.filter(targetMethod.parameterMap.contains).map(targetMethod.parameterMap)
                // We only pass non-local facts (i.e. field taints) to callee, as well as params. All other taints are method-specific.
                val factsToPass = currentRelevantActivations.filter(f => f == IFDSZeroFact || f.asTaintVariable.isField) ++ parametersToTaint

                // Register the call node to be a jump-back-point
                environment.setMethodReturnsTo(targetMethod.methodIdentifier, currentTask.method, currentNode)

                val targetRelevantActivations = environment.newActivationsAt(targetMethod.methodIdentifier, targetEntryBlock.stmtPc, factsToPass)

                // As for regular successors, we only queue the target method if it has new activations
                if(targetRelevantActivations.nonEmpty){
                  val targetResolverTask = ResolverTask(targetMethod.methodIdentifier, targetEntryBlock, targetRelevantActivations)
                  worklist.append(targetResolverTask)
                } else {
                  // If there are no new activations, we still need to propagate the method return facts along the
                  // call-to-return edges

                  // A method may taint fields -> we need to get those facts in the caller method
                  val relevantFactsFromMethod = environment
                    .methodResultingFacts(targetMethod.methodIdentifier)
                    .filter(f => f == IFDSZeroFact || f.asTaintVariable.isField)

                  // A method return may be tainted and assigned in the caller -> we need to taint the receiver
                  val returnFacts = if(environment.methodReturnIsTainted(targetMethod.methodIdentifier)){
                    currentNode
                      .allFactsInvolved
                      .find {
                        case tfr: TaintFunctionReturn if tfr.callPc == theCall.stmtPc => true
                        case _ => false
                      }
                      .map(tfr => Set(tfr) ++ currentNode.getFactsActivatedBy(tfr))
                      .getOrElse(Set.empty)
                  } else Set.empty


                  factsAfter = factsAfter ++ relevantFactsFromMethod ++ returnFacts
                }

              case None =>
              // If there is no statement in the method, then we don't execute it
            }
          }

        }

        if(currentNode.isReturnValue || currentNode.getSuccessors.isEmpty){
          // If this node is the end of the current method

          // We record the facts valid at the end of this method
          environment.setMethodReturnsFacts(currentTask.method, factsAfter)

          // We compute the set of facts that are valid to outside callers
          val relevantFactsFromMethod = factsAfter.filter(f => f == IFDSZeroFact || f.asTaintVariable.isField)

          // We iterate the set of callsites that invoke the current method and propagate our findings back to them
          environment
            .getMethodReturnsTo(currentTask.method)
            .getOrElse(Set.empty)
            .foreach{ case (callerMethod, callerNode) =>
              // Get facts that would be activated by tainted return of the current method in caller
              val returnFacts = if(environment.methodReturnIsTainted(currentTask.method)){
                callerNode
                  .allFactsInvolved
                  .find {
                    case tfr: TaintFunctionReturn if tfr.callPc == callerNode.stmtPc => true
                    case _ => false
                  }
                  .map(tfr => Set(tfr) ++ callerNode.getFactsActivatedBy(tfr))
                  .getOrElse(Set.empty)
              } else Set.empty

              // Queue all successor nodes in caller methods
              callerNode
                .getSuccessors
                .foreach{ callSuccessor =>
                  val allRelevantFactsInCaller = environment.newActivationsAt(callerMethod, callSuccessor.stmtPc, relevantFactsFromMethod ++ returnFacts)

                  if(allRelevantFactsInCaller.nonEmpty)
                    worklist.append(ResolverTask(callerMethod, callSuccessor, allRelevantFactsInCaller))
                }

            }
        }

        currentNode.getSuccessors.foreach{ succ =>
          worklist.append(ResolverTask(currentTask.method, succ, factsAfter))
        }
      }
    }

    environment.methodResultingFacts(methodIdent)
  }
}

object IFDSMethodRunner {
  private[reachability] type CallGraph = CallGraphBuilder#CallGraphView

  def apply(cg: CallGraph, summaryLookup: Map[MethodIdent, MethodIFDSRep]): IFDSMethodRunner = {
    val convertedLookup = Try( summaryLookup.view.mapValues(IFDSMethodGraph.apply).toMap )

    convertedLookup match {
      case Success(lookup) =>
        new IFDSMethodRunner(IFDSRunnerEnvironment(cg, lookup))
      case Failure(ex) =>
        throw new RuntimeException("Failed to convert IFDS summary to object representation", ex)
    }
  }
}
