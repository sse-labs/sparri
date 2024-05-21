package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.DefinedMethod

import scala.collection.mutable
import scala.util.Try

class DefaultRTACallGraphBuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends AbstractRTABuilder(programs, jreVersionToLoad){

  private[cg] final val priorInvocations: mutable.Map[DefinedMethod, Set[String]] = mutable.HashMap()

  override def buildFrom(dm: DefinedMethod): Try[CallGraphView] = {
    val types = if(dm.isStatic) Set.empty[String] else Set(dm.definingTypeName)

    resolve(dm, types)
  }

  private[cg] def resolve(entry: DefinedMethod, typesInstantiated: Set[String]): Try[CallGraphView] = Try {
    val workStack = mutable.Stack[ResolverTask]()
    val rootSet = mutable.Set.empty[String]


    do {
      workStack.push(ResolverTask(entry, typesInstantiated ++ rootSet.toSet, rootSet))

      while (workStack.nonEmpty) {
        val currTask = workStack.pop()

        val externalTypesPrior = priorInvocations.getOrElse(currTask.method, Set.empty)
        val newExternalTypes = currTask.typesInstantiated.diff(externalTypesPrior)

        if (!priorInvocations.contains(currTask.method) || newExternalTypes.nonEmpty) {

          rootSet.addAll(currTask.method.newTypesInstantiated)

          // What types do we have to look at?
          val effectiveTypesToResolve = currTask.method.newTypesInstantiated ++ newExternalTypes

          currTask.method.invocationStatements.foreach { jis =>
            resolveInvocation(jis, currTask.method, s => effectiveTypesToResolve.contains(s)).foreach { target =>
              putCall(currTask.method, jis.instructionPc, target)
              workStack.push(ResolverTask(target, effectiveTypesToResolve, rootSet))
            }
          }

          priorInvocations(currTask.method) = externalTypesPrior ++ effectiveTypesToResolve
        }
      }

    } while(rootSet.toSet.diff(priorInvocations(entry)).nonEmpty)


    getGraph
  }

  private[cg] case class ResolverTask(method: DefinedMethod, typesInstantiated: Set[String], newTypes: mutable.Set[String])

}
