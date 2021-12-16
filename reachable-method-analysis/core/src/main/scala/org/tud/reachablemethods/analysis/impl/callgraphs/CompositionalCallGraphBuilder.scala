package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.Method
import org.opalj.br.analyses.Project

import org.tud.reachablemethods.analysis.dataaccess.ElasticMethodData
import org.tud.reachablemethods.analysis.impl.CompositionalAnalysisContext

import java.net.URL
import scala.collection.mutable
import scala.util.{Success, Try}

class CompositionalCallGraphBuilder(opalProject: Project[URL],
                                    context: CompositionalAnalysisContext) extends CallGraphBuilder(opalProject) {


  def buildCallGraph(): Try[Any] = {
    // Set of methods already processed
    val methodSignaturesSeen: mutable.HashSet[String] = new mutable.HashSet[String]()

    def printProgress(methodName: String, internal: Boolean): Unit ={
      if(methodSignaturesSeen.size % 1000 == 0){
        val internalStr = if(internal) "Project" else "Dependency"
        log.debug(s"Processing: #${methodSignaturesSeen.size} [$internalStr] $methodName\r")
      }
    }

    def processMethod(method: Method): Unit = {
      if(opalProject.isProjectType(method.classFile.thisType)) {
        processInternalMethod(method)
      } else
        processDependencyMethod(method)
    }

    def processDependencyMethod(method: Method): Unit = {
      context.getMethodBySignatureAndClass(method.fullyQualifiedSignature, method.classFile.fqn) match {
        case Some(methodData) =>
          processDependencyMethodData(methodData)
        case None =>
          log.error("No method data in index: " + method.fullyQualifiedSignature)
      }
    }

    def processDependencyMethodData(methodData: ElasticMethodData): Unit = {
      if(!methodSignaturesSeen.contains(methodData.signature)){
        printProgress(methodData.signature, false)

        methodSignaturesSeen.add(methodData.signature)

        // TODO: If external -> need to map to other artifact!
        val dependencyCallees = methodData.calleeSignatures.flatMap{ sig =>
          val res = context.signatureLookup(sig)

          if(res.isEmpty)
            log.error("Did not find callee signature in local index, although preloading of callees is enabled: " + sig)

          res
        }

        dependencyCallees
          .foreach(processDependencyMethodData)

        val obligationTargets = methodData.obligations.flatMap{ obligation =>
          context.resolveObligationInLibrary(obligation, methodData.analyzedLibrary)
            .getOrElse({
              //log.warn("Type not found while resolving obligation: " + obligation.declaredTypeName)
              Iterable.empty
            })
        }

        obligationTargets.foreach {
          case Left(method) =>
            if (!methodSignaturesSeen.contains(method.fullyQualifiedSignature)) {
              log.info("Recursing into project method via obligation: " + method.fullyQualifiedSignature)
              processInternalMethod(method)
            }
          case Right(methodData) =>
            if (!methodSignaturesSeen.contains(methodData.signature)) {
              log.info("Recursing into dependency method via obligation: " + methodData.signature)
              processDependencyMethodData(methodData)
            }
        }


      }

    }

    def processInternalMethod(method: Method): Unit = {
      if(!methodSignaturesSeen.contains(method.fullyQualifiedSignature)){
        printProgress(method.fullyQualifiedSignature, true)

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

    entryPoints.foreach(processInternalMethod)

    Success()
  }

  override protected def isTypeInstantiated(typeFqn: String): Boolean = context.isTypeInstantiated(typeFqn)
}
