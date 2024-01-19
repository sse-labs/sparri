package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaInvokeStatement, JavaNewInstanceStatement, JavaProgram}

import scala.collection.mutable
import scala.util.Try

class DefaultRTACallGraphBuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends AbstractRTABuilder(programs, jreVersionToLoad){

  private[cg] final val priorInvocations: mutable.Map[DefinedMethod, Set[String]] = mutable.HashMap()

  def isStaticMethod(dm: DefinedMethod): Boolean = classLookup(dm.definingTypeName).lookupMethod(dm.methodName, dm.descriptor).exists(_.isStatic)
  def resolveFrom(dm: DefinedMethod): Try[Set[String]] = {
    val types = if(isStaticMethod(dm)) Set.empty[String] else Set(dm.definingTypeName)

    resolve(dm, types).map(_ => Set.empty[String])
  }

  def resolveFrom(dm: DefinedMethod, instantiatedTypes: Set[String]): Try[Set[String]] = Try {
    val oldTypesInstantiated = priorInvocations.getOrElse(dm, Set.empty)
    val newTypesInstantiated = instantiatedTypes.diff(oldTypesInstantiated)

    // Analyze every method at least once - and every time a new type was instantiatable
    if(!priorInvocations.contains(dm) || newTypesInstantiated.nonEmpty){

      // Store the fact that this method is now going to be analyzed with this new set of instantiatable types
      // This needs to be done before recursive invocation to avoid loops!
      priorInvocations(dm) = newTypesInstantiated ++ oldTypesInstantiated

      dm.javaMethodOpt match {
        case Some(jm) =>

          val currentInstantiatedTypes = mutable.Set[String]()

          for(statement <- jm.statements){
            if(statement.isNewInstanceInstruction){
              val typeInstantiated = statement.asInstanceOf[JavaNewInstanceStatement].instantiatedTypeName
              if(!oldTypesInstantiated.contains(typeInstantiated) && !newTypesInstantiated.contains(typeInstantiated)){
                currentInstantiatedTypes.add(typeInstantiated)
              }
            } else if(statement.isMethodInvocationInstruction){
              val jis = statement.asInstanceOf[JavaInvokeStatement]

              val effectiveNewTypes = currentInstantiatedTypes.toSet ++ newTypesInstantiated
              val targets = resolveInvocation(jis, effectiveNewTypes)

              for(target <- targets){
                putCall(dm, jis.instructionPc, target)
                if(!priorInvocations.contains(target) || effectiveNewTypes.exists(newType => !priorInvocations(target).contains(newType))) {
                  val newInstantiatedTypes = resolveFrom(target, effectiveNewTypes).get
                  currentInstantiatedTypes.addAll(newInstantiatedTypes)
                }
              }
            }
          }

          currentInstantiatedTypes.toSet ++ newTypesInstantiated
        case None =>
          log.error(s"Failed to lookup actual definition site of defined method: ${dm.definingTypeName}->${dm.methodName}")
          Set.empty
      }


    } else {
      Set.empty
    }
  }

  def resolve(entry: DefinedMethod, typesInstantiated: Set[String]): Try[Unit] = Try {
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

          currTask.method.javaMethodOpt.foreach { jm =>
            jm.invocationStatements.foreach { jis =>
              resolveInvocation(jis, effectiveTypesToResolve).foreach { target =>
                putCall(currTask.method, jis.instructionPc, target)
                workStack.push(ResolverTask(target, effectiveTypesToResolve, rootSet))
              }
            }
          }

          priorInvocations(currTask.method) = externalTypesPrior ++ effectiveTypesToResolve
        }
      }

    } while(rootSet.toSet.diff(priorInvocations(entry)).nonEmpty)

  }

  case class ResolverTask(method: DefinedMethod, typesInstantiated: Set[String], newTypes: mutable.Set[String])

}
