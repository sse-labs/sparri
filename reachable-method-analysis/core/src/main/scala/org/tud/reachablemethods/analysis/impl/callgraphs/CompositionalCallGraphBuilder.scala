package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.tud.reachablemethods.analysis.dataaccess.ElasticMethodData
import org.tud.reachablemethods.analysis.impl.CompositionalAnalysisContext
import org.tud.reachablemethods.analysis.logging.AnalysisLogger

import java.net.URL
import scala.collection.mutable
import scala.util.{Success, Try}

class CompositionalCallGraphBuilder(opalProject: Project[URL],
                                    context: CompositionalAnalysisContext, log: AnalysisLogger) extends CallGraphBuilder(opalProject, log) {


  def buildCallGraph(): Try[Any] = {

    def printProgress(methodName: String, internal: Boolean): Unit ={
      if(context.numberOfMethodsSeen() % 1000 == 0){
        val internalStr = if(internal) "Project" else "Dependency"
        log.debug(s"Processing: #${context.numberOfMethodsSeen()} [$internalStr] $methodName\r")
      }
    }

    def processMethod(method: Method): Unit = {
      if(opalProject.isProjectType(method.classFile.thisType)) {
        processInternalMethod(method)
      } else
        processDependencyMethod(method)
    }

    def processDependencyMethod(method: Method): Unit = {

      val dataOpt = context.getMethodBySignatureAndClass(method.fullyQualifiedSignature, method.classFile.fqn)

      if(dataOpt.isDefined){
        processDependencyMethodData(dataOpt.get)
      } else {
        log.error("No method data in index: " + method.fullyQualifiedSignature)
      }
    }

    def processDependencyMethodData(methodData: ElasticMethodData): Unit = {
      if(!context.methodSeen(methodData.signature)){
        printProgress(methodData.signature, false)

        context.addMethodSeen(methodData.signature)

        val dependencyCallees = if(methodData.isExtern){
          log.error("Extern method data!")
          Iterable.empty
        } else {
          methodData.calleeSignatures.flatMap { sig =>
            val res = context.signatureLookup(sig)

            if(res.isEmpty)
              log.error("Did not find callee signature in local index, although preloading of callees is enabled: " + sig)

            res
          }
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
            if (!context.methodSeen(method.fullyQualifiedSignature)) {
              log.info("Recursing into project method via obligation: " + method.fullyQualifiedSignature)
              processInternalMethod(method)
            }
          case Right(methodData) =>
            if (!context.methodSeen(methodData.signature)) {
              log.info("Recursing into dependency method via obligation: " + methodData.signature)
              processDependencyMethodData(methodData)
            }
        }


      }

    }

    def processInternalMethod(method: Method): Unit = {
      if(!context.methodSeen(method.fullyQualifiedSignature)){
        printProgress(method.fullyQualifiedSignature, true)

        context.addMethodSeen(method.fullyQualifiedSignature)

        if(method.body.isDefined){
          getAllCallees(method, opalProject)
            .foreach{ callee =>
              if(!context.methodSeen(callee.fullyQualifiedSignature)){
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
