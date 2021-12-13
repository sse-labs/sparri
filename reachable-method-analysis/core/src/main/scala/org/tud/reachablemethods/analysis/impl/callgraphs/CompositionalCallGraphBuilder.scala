package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.{ClassHierarchy, Method}
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.cg.InitialEntryPointsKey
import org.opalj.br.instructions.{INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.reachablemethods.analysis.dataaccess.ElasticMethodData
import org.tud.reachablemethods.analysis.impl.CompositionalAnalysisContext

import java.net.URL
import scala.collection.mutable
import scala.util.{Success, Try}

class CompositionalCallGraphBuilder(opalProject: Project[URL], context: CompositionalAnalysisContext) {

  private val log: Logger = LoggerFactory.getLogger(getClass)

  def entryPoints: Traversable[Method] =
    opalProject.get(InitialEntryPointsKey).filter(m => opalProject.isProjectType(m.classFile.thisType))

  def classHierarchy: ClassHierarchy = opalProject.classHierarchy

  def buildCallGraph(): Try[Any] = {
    // Set of methods already processed
    val methodSignaturesSeen: mutable.HashSet[String] = new mutable.HashSet[String]()

    def processMethod(method: Method): Unit = {
      if(opalProject.isProjectType(method.classFile.thisType)) {
        processInternalMethod(method)
      } else
        processDependencyMethod(method)
    }

    def processDependencyMethod(method: Method): Unit = {
      methodSignaturesSeen.add(method.fullyQualifiedSignature)

      context.getMethodBySignatureAndClass(method.fullyQualifiedSignature, method.classFile.fqn) match {
        case Some(methodData) =>
          processDependencyMethodData(methodData)
        case None =>
          log.error("No method data in index: " + method.fullyQualifiedSignature)
      }
    }

    def processDependencyMethodData(methodData: ElasticMethodData): Unit = {
      log.debug("Processing dependency: " + methodData.signature)
      methodSignaturesSeen.add(methodData.signature)

      // TODO: If external -> need to map to other artifact!
      val dependencyCallees = context.getInvokedMethods(methodData.elasticId, methodData.libraryVersion)

      dependencyCallees
        .filter(data => !methodSignaturesSeen.contains(data.signature))
        .foreach(processDependencyMethodData)

      dependencyCallees.foreach{ callee => {
        callee.obligations.foreach { obligation =>
          //TODO: Use index for type lookup
          opalProject.allClassFiles.find(_.thisType.fqn.equals(obligation.declaredTypeName)) match {
            case Some(declaredType) =>
              classHierarchy.allSubtypes(declaredType.thisType, reflexive = false)
                .filter(t => opalProject.isProjectType(t)) //TODO: Not true, need to find subtypes in other libraries as well..all libraries except current one!
                .filter(t => context.isTypeInstantiated(t.fqn))
                .flatMap(t => opalProject.allMethods.filter(m => m.classFile.thisType.equals(t)))
                .filter(m => (m.name + m.descriptor.valueToString).equals(obligation.methodDescription))
                .filter(m => !methodSignaturesSeen.contains(m.fullyQualifiedSignature))
                .foreach{ method =>
                  log.info("Recursing into project method via obligation: " + method.fullyQualifiedSignature)
                  processMethod(method)
                }

            case None =>
              log.error("Declared type of obligation was not found in project type hierarchy: " + obligation.declaredTypeName)
          }
        }
      }}
    }

    def processInternalMethod(method: Method): Unit = {
      log.debug("Processing internal: " + method.fullyQualifiedSignature)
      methodSignaturesSeen.add(method.fullyQualifiedSignature)

      if(method.body.isDefined){
        getAllCallees(method, opalProject)
          .foreach{ callee =>
            if(!methodSignaturesSeen.contains(callee.fullyQualifiedSignature)){
              processMethod(callee)
            }
          }
      }
    }

    entryPoints.foreach(processInternalMethod)

    Success()
  }

  /**
   * This method processes a method with body, iterates all method invocation instructions and resolves the call targets
   * via the project interface. The result is an array of methods that are being invoked by the given method. The result
   * may contain duplicates.
   * @param method Method to resolver callees for. Must have a non-empty body!
   * @param project Project context for resolving invocations
   * @return Array of potential target methods, may contain duplicates
   */
  private[callgraphs] def getAllCallees(method: Method, project: Project[URL]): Array[Method] = {

    assert(method.body.isDefined)
    method
      .body
      .get
      .instructions
      .filter(instr => instr != null && instr.isMethodInvocationInstruction)
      .flatMap {
        case iv: INVOKEVIRTUAL =>
          project.virtualCall(method.classFile.thisType, iv)
            .filter(m => context.isTypeInstantiated(m.classFile.thisType.fqn))
          //TODO: Check empty results
        case is: INVOKESTATIC =>
          val resolvedCall = project.staticCall(method.classFile.thisType, is)

          if(resolvedCall.hasValue){
            Traversable(resolvedCall.value)
          } else {
            log.warn("Failed to resolve static call " + is.toString())
            log.warn("\t- Call contained in " + method.fullyQualifiedSignature)
            Traversable.empty
          }

        case special: INVOKESPECIAL =>
          val resolvedCall = project.specialCall(method.classFile.thisType, special)

          if(resolvedCall.hasValue){
            Traversable(resolvedCall.value)
          } else {
            log.warn("Failed to resolve special call " + special.toString())
            log.warn("\t- Call contained in " + method.fullyQualifiedSignature)
            Traversable.empty
          }

        case interface: INVOKEINTERFACE =>
          project.interfaceCall(method.classFile.thisType, interface)
            .filter(m => context.isTypeInstantiated(m.classFile.thisType.fqn))
          //TODO: Check empty results
      }
  }

}