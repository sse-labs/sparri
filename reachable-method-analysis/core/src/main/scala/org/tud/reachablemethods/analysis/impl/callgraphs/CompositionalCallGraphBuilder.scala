package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.tud.reachablemethods.analysis.dataaccess.{ElasticMethodData, InvocationObligation}
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

    val methodsToResolve: mutable.ListBuffer[Either[Method, ElasticMethodData]] = new mutable.ListBuffer

    def processMethod(method: Either[Method, ElasticMethodData]): Unit = {

      if(method.isRight){
        val data = method.right.get

        if(!context.methodSeen(data.signature)) {
          processDependencyMethodData(data)
        }
      } else {
        val methodObj = method.left.get

        if(!context.methodSeen(methodObj.fullyQualifiedSignature)){
          if(opalProject.isProjectType(methodObj.classFile.thisType)){
            processInternalMethod(methodObj)
          } else {
            processDependencyMethod(methodObj)
          }
        }
      }
    }


    def processDependencyMethod(method: Method): Unit = {

      val methodOpt = context.signatureLookup(method.fullyQualifiedSignature)

      if(methodOpt.isDefined){
        methodOpt.get match {
          case Left(method) =>
            processInternalMethod(method)// TODO: We now have JRE Methods with no elastic data. Should be treated like internal methods?
          case Right(methodData) =>
            processDependencyMethodData(methodData)
        }
      } else {
        log.error("No method data in index: " + method.fullyQualifiedSignature)
      }
    }

    def processDependencyMethodData(methodData: ElasticMethodData): Unit = {

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
        .foreach(d => methodsToResolve.append(d))

      /*methodData.obligations.foreach{ obligation =>
        if(!context.obligationResolved(obligation, methodData.analyzedLibrary)){
          context.resolveObligationInLibrary(obligation, methodData.analyzedLibrary)
            .getOrElse({
              //log.warn("Type not found while resolving obligation: " + obligation.declaredTypeName)
              Iterable.empty
            })
            .foreach(d => methodsToResolve.append(d))
        }
      }*/
      //TODO: Reintroduce method obligation resolving once context is adapted



    }

    def processInternalMethod(method: Method): Unit = {
      printProgress(method.fullyQualifiedSignature, true)

      context.addMethodSeen(method.fullyQualifiedSignature)

      if(method.body.isDefined){
        getAllCallees(method, opalProject)
          .foreach{ callee =>
            if(!context.methodSeen(callee.fullyQualifiedSignature)){
              methodsToResolve.append(Left(callee))
            }
          }
      }
    }

    entryPoints.foreach(m => methodsToResolve.append(Left(m)))

    while(methodsToResolve.nonEmpty){
      val m = methodsToResolve.head
      methodsToResolve.remove(0)

      processMethod(m)
    }

    Success()
  }

  override protected def isTypeInstantiated(typeFqn: String): Boolean = context.isTypeInstantiated(typeFqn)
}
