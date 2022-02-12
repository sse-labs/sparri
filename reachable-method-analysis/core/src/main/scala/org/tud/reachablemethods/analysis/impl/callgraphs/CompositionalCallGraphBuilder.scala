package org.tud.reachablemethods.analysis.impl.callgraphs

import org.opalj.br.Method
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL}
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

      methodData.obligations.foreach{ obligation =>
        if(!context.obligationResolved(obligation, methodData.analyzedLibrary)){

          val obligationTargets: Iterable[context.MethodInformation] = context
            .resolveObligationInLibrary(obligation, methodData.analyzedLibrary)
            .getOrElse(Iterable.empty)

          obligationTargets.foreach{ target =>
            methodsToResolve.append(target)
          }
        }
      }
    }

    def processInternalMethod(method: Method): Unit = {
      printProgress(method.fullyQualifiedSignature, true)

      context.addMethodSeen(method.fullyQualifiedSignature)

      if(method.body.isDefined){
        getAllCalleesCompositional(method, opalProject)
          .foreach { callee =>
            val signature = if(callee.isLeft) callee.left.get.fullyQualifiedSignature else callee.right.get.signature

            if(!context.methodSeen(signature)){
              methodsToResolve.append(callee)
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


  private def getAllCalleesCompositional(method: Method, project: Project[URL]): Array[context.MethodInformation] = {
    assert(method.body.isDefined)

    method
      .body
      .get
      .instructions
      .filter(instr => instr != null && instr.isMethodInvocationInstruction)
      .flatMap {
        case iv: INVOKEVIRTUAL =>
          val projectHits: Iterable[context.MethodInformation] = project
            .virtualCall(method.classFile.thisType, iv)
            .filter(m => isTypeInstantiated(m.classFile.thisType.fqn))
            .map(Left(_))

          //TODO: Does 'asObjectType' work here?
          context.resolveVirtual(iv.declaringClass.asObjectType.fqn, iv.methodDescriptor.valueToString) match {
            case Some(contextHits) => projectHits ++ contextHits
            case None =>
              log.warn(s"No context hits for virtual resolve: $iv")
              projectHits
          }

        case is: INVOKESTATIC =>
          val resolvedCall = project.staticCall(method.classFile.thisType, is)

          if(resolvedCall.hasValue){
            Traversable(Left(resolvedCall.value))
          } else {
            val contextResolve = context.resolveNonVirtual(is.declaringClass.fqn, is.methodDescriptor.valueToString)

            if(contextResolve.isDefined){
              Traversable(contextResolve.get)
            } else {
              log.warn("Failed to resolve static call " + is.toString())
              log.warn("\t- Call contained in " + method.fullyQualifiedSignature)
              Traversable.empty
            }
          }

        case special: INVOKESPECIAL =>

          val resolvedCall = project.specialCall(method.classFile.thisType, special)

          if(resolvedCall.hasValue){
            Traversable(Left(resolvedCall.value))
          } else {

            val contextResolve = context.resolveNonVirtual(special.declaringClass.fqn, special.methodDescriptor.valueToString)

            if(contextResolve.isDefined){
              Traversable(contextResolve.get)
            } else {
              log.warn("Failed to resolve special call " + special.toString())
              log.warn("\t- Call contained in " + method.fullyQualifiedSignature)
              Traversable.empty
            }

          }

        case interface: INVOKEINTERFACE =>
          val projectHits: Iterable[context.MethodInformation] = project
            .interfaceCall(method.classFile.thisType, interface)
            .filter(m => isTypeInstantiated(m.classFile.thisType.fqn))
            .map(Left(_))

          context.resolveVirtual(interface.declaringClass.fqn, interface.methodDescriptor.valueToString) match {
            case Some(contextHits) => projectHits ++ contextHits
            case None =>
              log.warn(s"No context hits for interface resolve: $interface")
              projectHits
          }
      }
  }
}
