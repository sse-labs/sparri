package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram

import scala.collection.mutable
import scala.util.Try

class NaiveRTACallGraphBuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends AbstractRTABuilder(programs, jreVersionToLoad){

  private[cg] val allInstantiatedTypes = programs
    .flatMap(_.allMethods)
    .flatMap(_.newStatements)
    .map(_.instantiatedTypeName) ++ jreOpt.map(_.allTypesInstantiated).getOrElse(Set.empty[String])

  def buildNaive(): Try[CallGraphView] = Try {

    // Create a sequence in which first all program classes will be resolved, then all JRE classes
    val toResolve = programs.flatMap(_.allClasses).toSeq ++ jreOpt.map(_.types.map(_.asModel).toSeq).getOrElse(Seq.empty)

    toResolve
      .foreach{ javaClass =>

        javaClass.getMethods.foreach{ javaMethod =>
          val currentDefinedMethod = asDefinedMethod(javaMethod)
          javaMethod.invocationStatements.foreach { jis =>

            resolveInvocation(jis, allInstantiatedTypes, simpleCaching = true)
              .foreach(targetDefinedMethod => putCall(currentDefinedMethod, jis.instructionPc, targetDefinedMethod))
          }
        }
      }

    getGraph
  }

  private[cg] val methodsAnalyzed = mutable.HashSet[Int]()

  override def buildFrom(dm: DefinedMethod): Try[CallGraphView] = Try {
    val workList = mutable.Queue[DefinedMethod](dm)

    while(workList.nonEmpty){
      val currentMethod = workList.dequeue()

      if(!methodsAnalyzed.contains(currentMethod.hashCode())){

        currentMethod.invocationStatements.foreach { jis =>
          resolveInvocation(jis, allInstantiatedTypes, simpleCaching = true).foreach { target =>
            putCall(currentMethod, jis.instructionPc, target)
            if (!methodsAnalyzed.contains(target.hashCode()))
              workList.enqueue(target)
          }
        }

        methodsAnalyzed.add(currentMethod.hashCode())
      }
    }

    getGraph
  }

}
