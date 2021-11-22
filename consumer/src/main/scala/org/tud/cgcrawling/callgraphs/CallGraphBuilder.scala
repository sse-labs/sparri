package org.tud.cgcrawling.callgraphs

import akka.actor.ActorSystem
import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.cg.InitialEntryPointsKey
import org.opalj.br.instructions.{INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.download.MavenDownloadResult
import org.tud.cgcrawling.model.LibraryCallgraph
import org.tud.cgcrawling.opal.OPALProjectHelper
import org.tud.cgcrawling.opal.OPALProjectHelper.ClassList

import java.net.URL
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CallGraphBuilder(val config: Configuration, val system: ActorSystem) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

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

        case is: INVOKESTATIC =>
          val resolvedCall = project.staticCall(method.classFile.thisType, is)

          if(resolvedCall.hasValue){
            Traversable(resolvedCall.value)
          } else {
            log.warn("Failed to resolve static call " + is.toString())
            Traversable.empty
          }

        case special: INVOKESPECIAL =>
          val resolvedCall = project.specialCall(method.classFile.thisType, special)

          if(resolvedCall.hasValue){
            Traversable(resolvedCall.value)
          } else {
            log.warn("Failed to resolve special call " + special.toString())
            Traversable.empty
          }

        case interface: INVOKEINTERFACE =>
          project.interfaceCall(method.classFile.thisType, interface)
      }
  }

  /**
   * Builds the LibraryCallgraph object model for a fully initialized OPAL project. Failures when resolving method calls
   * will yield console warnings, but the callgraph will be constructed regardless.
   *
   * @param project OPAL project instance with JRE implementations and 3rd party interfaces included
   * @param projectIdent MavenIdentifier of the current project
   * @param classFqnToIdentMap Map for associating Classfile FQNs with their corresponding MavenIdentifier. Allows mapping 3rd party methods to their library
   * @return A LibraryCallgraph instance
   */
  private[callgraphs] def buildCgObjectModel(project: Project[URL], projectIdent: MavenIdentifier, classFqnToIdentMap: Map[String, MavenIdentifier]): LibraryCallgraph = {

    // The set of entry points. The project configuration set in the OPALProjectHelper enforces usage of the
    // LibraryEntryPointsFinder
    val projectEntryPoints = project.get(InitialEntryPointsKey)
      .filter(m => project.allProjectClassFiles.contains(m.classFile) && m.body.isDefined)

    // Set of method signatures that the algorithm has already processed. This is needed to avoid running into endless
    // loops when analyzing recursive invocations. Since we are loading the entire CP of the project, Signatures should
    // be unique.
    val methodSignaturesSeen: mutable.HashSet[String] = new mutable.HashSet[String]()

    // The LibraryCallgraph instance that will be iteratively constructed and returned at the end of this method
    val callgraph: LibraryCallgraph = new LibraryCallgraph

    /**
     * Callback that is invoked whenever the callgraph algorithm found a new method
     * @param method The OPAL method instance
     */
    def handleMethod(method: Method): Unit = {
      // This method is only invoked for project methods, so we can fix 'isExternal=false'
      callgraph.addMethod(method, isExternal = false, Some(projectIdent))
    }

    /**
     * Callback that is invoked whenever the callgraph algorithm found a new invocation / edge. The algorithm guarantees
     * that `handleMethod` has already been invoked for the caller at this point. However, the callee may be an entirely
     * new method
     * @param caller OPAL method instance representing the caller
     * @param callee OPAL method instance representing the callee
     */
    def handleEdge(caller: Method, callee: Method): Unit = {
      // We know that 'caller' has been processed already. Callee may be external or not
      val isExternal = !project.isProjectType(callee.classFile.thisType)
      val definingArtifact = if (isExternal) classFqnToIdentMap.get(callee.classFile.fqn) else Some(projectIdent)

      callgraph.addEdge(caller, callee, isExternal, definingArtifact)
    }

    /**
     * Recursive implementation of the callgraph generation algorithm. Processes a method, adds it to the callgraph,
     * extracts callees and invokes itself recursively on *project-internal* callees. The important part is that the
     * recursive exploration of reachable methods is cut off as soon as external methods (= library invocations) are found.
     * @param method The method to analyze
     */
    def processMethod(method: Method): Unit = {
      // Process a new method
      handleMethod(method)
      methodSignaturesSeen.add(method.fullyQualifiedSignature)

      // Callees can only be detected for methods that have a body
      if(method.body.isDefined){
        getAllCallees(method, project)
          .foreach{ callee =>
            // Process a new edge
            handleEdge(method, callee)

            // Recursively process the callee only if a) we haven't processed it yet and b) it is a project method
            if(!methodSignaturesSeen.contains(callee.fullyQualifiedSignature) && project.isProjectType(callee.classFile.thisType)){
              processMethod(callee)
            }
          }
      }
    }

    // Start discovery for each entry point
    projectEntryPoints.foreach(processMethod)

    callgraph
  }

  def buildCallgraph(jarFile: MavenDownloadResult, thirdPartyClasses: ClassList, classFqnToIdentMap: Map[String, MavenIdentifier]): CallGraphBuilderResult = {

    val projectClasses =
      OPALProjectHelper.readClassesFromJarStream(jarFile.jarFile.get.is, jarFile.identifier.toJarLocation.toURL, loadImplementation = true).get

    jarFile.jarFile.get.is.close()


    Try(OPALProjectHelper.buildOPALProject(projectClasses, thirdPartyClasses)) match {
      case Success(project) =>
        log.info(s"Successfully initialized OPAL project for ${jarFile.identifier.toString}")

        Try(buildCgObjectModel(project, jarFile.identifier, classFqnToIdentMap)) match {
          case Success(callgraph) =>
            log.info(s"Successfully generated Callgraph with ${callgraph.numberOfReachableMethods()} reachable methods for ${jarFile.identifier.toString}")
            CallGraphBuilderResult(jarFile.identifier, success = true, Some(callgraph))
          case Failure(ex) =>
            log.error(s"Failed to generate Callgraph for ${jarFile.identifier.toString}", ex)
            CallGraphBuilderResult(jarFile.identifier, success = false, None)
        }
      case Failure(ex) =>
        log.error(s"Error while analyzing JAR for artifact ${jarFile.identifier.toString}", ex)
        CallGraphBuilderResult(jarFile.identifier, success = false, None)
    }
  }
}


case class CallGraphBuilderResult(identifier: MavenIdentifier,
                                  success: Boolean,
                                  callgraph: Option[LibraryCallgraph])