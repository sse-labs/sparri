package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram

import scala.util.Try

class NaiveRTACallGraphBuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends AbstractRTABuilder(programs, jreVersionToLoad){

  def buildNaive(): Try[Unit] = Try {

    val allInstantiatedTypes = programs
      .flatMap(_.allMethods)
      .flatMap(_.getNewStatements)
      .map(_.instantiatedTypeName) ++ jreOpt.map(_.allTypesInstantiated).getOrElse(Set.empty[String])

    // Create a sequence in which first all program classes will be resolved, then all JRE classes
    val toResolve = programs.flatMap(_.allClasses).toSeq ++ jreOpt.map(_.types.map(_.asModel).toSeq).getOrElse(Seq.empty)

    toResolve
      .foreach{ javaClass =>

        javaClass.getMethods.foreach{ javaMethod =>
          val currentDefinedMethod = asDefinedMethod(javaMethod)
          javaMethod.getInvocationStatements.foreach { jis =>

            resolveInvocation(jis, allInstantiatedTypes)
              .foreach(targetDefinedMethod => putCall(currentDefinedMethod, jis.instructionPc, targetDefinedMethod))
          }
        }
      }
  }

  def getGraph(): Any = ???

}
