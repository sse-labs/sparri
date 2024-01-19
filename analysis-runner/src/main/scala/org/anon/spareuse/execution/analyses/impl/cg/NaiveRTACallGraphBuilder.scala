package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram

import scala.collection.mutable
import scala.util.Try

class NaiveRTACallGraphBuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends AbstractRTABuilder(programs, jreVersionToLoad){

  private[cg] val allInstantiatedTypes = programs
    .flatMap(_.allMethods)
    .flatMap(_.newStatements)
    .map(_.instantiatedTypeName) ++ jreOpt.map(_.allTypesInstantiated).getOrElse(Set.empty[String])

  def buildNaive(): Try[Unit] = Try {

    // Create a sequence in which first all program classes will be resolved, then all JRE classes
    val toResolve = programs.flatMap(_.allClasses).toSeq ++ jreOpt.map(_.types.map(_.asModel).toSeq).getOrElse(Seq.empty)

    toResolve
      .foreach{ javaClass =>

        javaClass.getMethods.foreach{ javaMethod =>
          val currentDefinedMethod = asDefinedMethod(javaMethod)
          javaMethod.invocationStatements.foreach { jis =>

            resolveInvocation(jis, allInstantiatedTypes)
              .foreach(targetDefinedMethod => putCall(currentDefinedMethod, jis.instructionPc, targetDefinedMethod))
          }
        }
      }
  }

  private[cg] val methodsAnalyzed = mutable.Set[DefinedMethod]()

  def buildNaiveFrom(dm: DefinedMethod): Try[Unit] = Try {
    val workList = mutable.Queue[DefinedMethod](dm)

    while(workList.nonEmpty){
      val currentMethod = workList.dequeue()

      if(!methodsAnalyzed.contains(currentMethod)){

        currentMethod.javaMethodOpt match {
          case Some(jm) =>
            jm.invocationStatements.foreach{ jis =>
              resolveInvocation(jis, allInstantiatedTypes).foreach{ target =>
                putCall(currentMethod, jis.instructionPc, target)
                if(!methodsAnalyzed.contains(target))
                  workList.enqueue(target)
              }
            }
          case None =>
            log.error(s"Failed to lookup actual definition site of defined method: ${currentMethod.definingTypeName}->${currentMethod.methodName}")
        }


        methodsAnalyzed.add(currentMethod)
      }
    }
  }

}
