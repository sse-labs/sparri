package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.{ClassHierarchy, Method}
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.cg.InitialEntryPointsKey
import org.opalj.br.instructions.{INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL}
import org.tud.reachablemethods.analysis.logging.{AnalysisLogger, AnalysisLogging}

import java.net.URL

abstract class CallGraphBuilder(opalProject: Project[URL], override val log: AnalysisLogger) extends AnalysisLogging {

  protected val classHierarchy: ClassHierarchy = opalProject.classHierarchy

  protected def entryPoints: Traversable[Method] =
    opalProject.get(InitialEntryPointsKey).filter(m => opalProject.isProjectType(m.classFile.thisType))

  protected def isTypeInstantiated(typeFqn: String): Boolean

  /**
   * This method processes a method with body, iterates all method invocation instructions and resolves the call targets
   * via the project interface. The result is an array of methods that are being invoked by the given method. The result
   * may contain duplicates.
   * @param method Method to resolver callees for. Must have a non-empty body!
   * @param project Project context for resolving invocations
   * @return Array of potential target methods, may contain duplicates
   */
  protected[callgraphs] def getAllCallees(method: Method, project: Project[URL]): Array[Method] = {
    assert(method.body.isDefined)
    method
      .body
      .get
      .instructions
      .filter(instr => instr != null && instr.isMethodInvocationInstruction)
      .flatMap {
        case iv: INVOKEVIRTUAL =>
          project.virtualCall(method.classFile.thisType, iv)
            .filter(m => isTypeInstantiated(m.classFile.thisType.fqn))
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
            .filter(m => isTypeInstantiated(m.classFile.thisType.fqn))
        //TODO: Check empty results
      }
  }

}

object CallGraphBuilder {

  def getInstantiatedTypeNames(project: Project[URL], projectOnly: Boolean): Set[String] = {
    project
      .allMethods
      .filter(m => (!projectOnly || project.isProjectType(m.classFile.thisType)) && m.body.isDefined)
      .flatMap(m => m.body.get.instructions)
      .filter(i => i != null && i.isMethodInvocationInstruction && i.isInstanceOf[INVOKESPECIAL])
      .map(i => i.asInstanceOf[INVOKESPECIAL])
      .filter(i => i.name.equals("<init>"))
      .map(_.declaringClass.fqn)
      .toSet
  }


}
