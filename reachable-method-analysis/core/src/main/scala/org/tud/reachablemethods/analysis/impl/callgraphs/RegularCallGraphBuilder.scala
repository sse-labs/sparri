package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.tud.reachablemethods.analysis.logging.AnalysisLogger

import java.net.URL
import scala.collection.mutable
import scala.util.Try

class RegularCallGraphBuilder(opalProject: Project[URL], log: AnalysisLogger) extends CallGraphBuilder(opalProject, log) {

  private[callgraphs] val instantiatedTypes = CallGraphBuilder.getInstantiatedTypeNames(opalProject, projectOnly = false)

  override protected def isTypeInstantiated(typeFqn: String): Boolean = instantiatedTypes.contains(typeFqn)



  def calculateMethodsReachable(): Try[Iterable[String]] = Try {
    // Set of methods already processed
    val methodSignaturesSeen: mutable.HashSet[String] = new mutable.HashSet[String]()

    def printProgress(methodName: String, internal: Boolean): Unit ={
      if(methodSignaturesSeen.size % 1000 == 0){
        val internalStr = if(internal) "Project" else "Dependency"
        log.debug(s"Processing: #${methodSignaturesSeen.size} [$internalStr] $methodName\r")
      }
    }

    def processMethod(method: Method): Unit = {
      if(!methodSignaturesSeen.contains(method.fullyQualifiedSignature)){
        printProgress(method.fullyQualifiedSignature, true)
        //log.debug("Processing internal: " + method.fullyQualifiedSignature)
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
    }

    entryPoints.foreach(processMethod)

    methodSignaturesSeen
  }
}
