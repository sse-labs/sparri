package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaInvokeStatement, JavaNewInstanceStatement, JavaProgram}

import scala.collection.mutable
import scala.util.Try

class DefaultRTACallGraphBuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends AbstractRTABuilder(programs, jreVersionToLoad){

  private[cg] final val priorInvocations: mutable.Map[DefinedMethod, Set[String]] = mutable.HashMap()

  def resolveFrom(dm: DefinedMethod): Try[Set[String]] = resolveFrom(dm, Set.empty)

  def resolveFrom(dm: DefinedMethod, instantiatedTypes: Set[String]): Try[Set[String]] = Try {
    val newTypesInstantiated = instantiatedTypes.diff(priorInvocations.getOrElse(dm, Set.empty))

    // Analyze every method at least once - and every time a new type was instantiatable
    if(!priorInvocations.contains(dm) || newTypesInstantiated.nonEmpty){

      // Store the fact that this method is now going to be analyzed with this new set of instantiatable types
      // This needs to be done before recursive invocation to avoid loops!
      priorInvocations(dm) = newTypesInstantiated ++ priorInvocations.getOrElse(dm, Set.empty)

      getJavaMethod(dm) match {
        case Some(jm) =>

          val currentInstantiatedTypes = mutable.Set[String]()

          jm.getStatements.foreach{
            // Only track new instantiations that we have not seen before
            case jnis: JavaNewInstanceStatement if !priorInvocations.get(dm).exists(_.contains(jnis.instantiatedTypeName)) =>
              currentInstantiatedTypes.add(jnis.instantiatedTypeName)

            case jis: JavaInvokeStatement =>

              //TODO: Check that this is valid, i.e. okay to only consider the newly instantiated types instead of all types instantiated at this point
              val currTypes = currentInstantiatedTypes.toSet
              val targets = resolveInvocation(jis, currTypes ++ newTypesInstantiated)

              //TODO: THis is wrong! We not only need to look at new targets, but also old targets for which new types have been discovered!
              //TODO: Stack overflows!
              val newTargets = targets
                .filter{ t =>
                  if(calleesOf.contains(dm) && calleesOf(dm).contains(jis.instructionPc) && calleesOf(dm)(jis.instructionPc).contains(t))
                    ! priorInvocations.get(t).exists( prev => (currTypes ++ newTypesInstantiated).forall(s => prev.contains(s)))
                  else true
                }

              //val newTargets = targets.diff(calleesOf.get(dm).flatMap(_.get(jis.instructionPc)).getOrElse(Set.empty))

              newTargets.foreach(target => putCall(dm, jis.instructionPc, target))

              newTargets.foreach{ newTarget =>
                val additionalNewTypes = resolveFrom(newTarget, currTypes ++ newTypesInstantiated).get

                currentInstantiatedTypes.addAll(additionalNewTypes)
              }


            case _ =>

          }

          currentInstantiatedTypes.toSet
        case None =>
          log.error(s"Failed to lookup actual definition site of defined method: ${dm.definingTypeName}->${dm.methodName}")
          Set.empty
      }


    } else {
      Set.empty
    }

  }


}
