package org.tud.cgcrawling.model

import org.opalj.br.{DeclaredMethod, VirtualDeclaredMethod}
import org.opalj.br.analyses.Project
import org.opalj.tac.cg.CallGraph
import org.slf4j.{Logger, LoggerFactory}

import java.net.URL
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class LibraryCallGraphEvolution(val groupId: String, val artifactId: String) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val methodEvolutionMap: mutable.Map[MethodIdentifier, MethodEvolution] = new mutable.HashMap()
  private val invocationEvolutionMap: mutable.Map[MethodInvocationIdentifier, MethodInvocationEvolution] =
    new mutable.HashMap()

  private val releaseList: mutable.ListBuffer[String] = new ListBuffer[String]

  val libraryName = s"$groupId:$artifactId"

  def releases(): List[String] = releaseList.toList
  def methodEvolutions(): Iterable[MethodEvolution] = methodEvolutionMap.values
  def methodInvocationEvolutions(): Iterable[MethodInvocationEvolution] = invocationEvolutionMap.values

  def numberOfMethodEvolutions(): Int = methodEvolutionMap.size
  def numberOfInvocationEvolutions(): Int = invocationEvolutionMap.size

  def methodsAt(release: String): Iterable[MethodIdentifier] = {
    if(!releaseList.contains(release))
      throw new RuntimeException(s"Unknown release $release")

    methodEvolutions()
      .filter(_.isActiveIn.contains(release))
      .map(_.identifier)
  }

  def calleesAt(caller: MethodIdentifier, release: String): Iterable[MethodIdentifier] ={
    if(!releaseList.contains(release))
      throw new RuntimeException(s"Unknown release $release")

    if(!methodEvolutionMap.contains(caller) || !methodEvolutionMap(caller).isActiveIn.contains(release))
      throw new RuntimeException(s"Method $caller was not active in release $release")

    invocationEvolutionMap
      .keys
      .filter(_.calleeIdent.equals(caller))
      .filter(invocationEvolutionMap(_).isActiveIn.contains(release))
      .map(_.calleeIdent)
      .toList
  }

  def applyNewRelease(callgraph: CallGraph, project: Project[URL], release: String): Unit = {
    if(releaseList.contains(release)){
      throw new RuntimeException(s"Release has already been applied to CallGraphEvolution: $release")
    }

    log.info(s"Processing new release $release for $libraryName")
    releaseList.append(release)

    val methods = callgraph.reachableMethods().toSet
    var cnt: Int = 0
    var invocationCnt: Int = 0

    methods
      .foreach { method =>
        val isExternal: Boolean = isExternalMethod(method, project)
        val callerIdent = MethodIdentifier.fromOpalMethod(method, isExternal)

        if(setMethodActiveInRelease(callerIdent, release)){
          cnt += 1
        }

        callgraph
          .calleesOf(method)
          .flatMap(_._2)
          .map(m => MethodIdentifier.fromOpalMethod(m, isExternalMethod(m, project)))
          .toList
          .distinct
          .foreach{ calleeIdent =>
            if(setMethodActiveInRelease(calleeIdent, release)) cnt += 1

            val ident = new MethodInvocationIdentifier(callerIdent, calleeIdent)
            if(setInvocationActiveInRelease(ident, release)) invocationCnt += 1
          }
      }
  }

  private def setInvocationActiveInRelease(identifier: MethodInvocationIdentifier, release: String): Boolean = {
    var wasNewInvocation = false

    if(!invocationEvolutionMap.contains(identifier)){
      invocationEvolutionMap.put(identifier, new MethodInvocationEvolution(identifier))
      wasNewInvocation = true
    }

    invocationEvolutionMap(identifier).addActiveRelease(release)

    wasNewInvocation
  }

  private def setMethodActiveInRelease(identifier: MethodIdentifier, release: String): Boolean = {
    var wasNewMethod = false

    if(!methodEvolutionMap.contains(identifier)){
      methodEvolutionMap.put(identifier, new MethodEvolution(identifier))
      wasNewMethod = true
    }

    methodEvolutionMap(identifier).addActiveRelease(release)

    wasNewMethod
  }

  private def isExternalMethod(method: DeclaredMethod, project: Project[URL]): Boolean = {
    method.isInstanceOf[VirtualDeclaredMethod] ||
      !project.allProjectClassFiles.map(_.thisType).contains(method.declaringClassType)
  }
}



