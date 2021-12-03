package org.tud.cgcrawling.model

import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class LibraryCallGraphEvolution(val groupId: String, val artifactId: String) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val methodEvolutionMap: mutable.Map[MethodIdentifier, MethodEvolution] = new mutable.HashMap()
  private val invocationEvolutionMap: mutable.Map[MethodInvocationIdentifier, MethodInvocationEvolution] =
    new mutable.HashMap()
  private val dependencyEvolutionMap: mutable.Map[DependencyIdentifier, DependencyEvolution] = new mutable.HashMap()


  private val releaseList: mutable.ListBuffer[String] = new ListBuffer[String]
  private val instantiatedTypesMap: mutable.Map[String, mutable.ListBuffer[String]] = new mutable.HashMap()

  val libraryName = s"$groupId:$artifactId"

  def releases(): List[String] = releaseList.toList
  def methodEvolutions(): Iterable[MethodEvolution] = methodEvolutionMap.values
  def methodInvocationEvolutions(): Iterable[MethodInvocationEvolution] = invocationEvolutionMap.values
  def dependencyEvolutions(): Iterable[DependencyEvolution] = dependencyEvolutionMap.values

  def numberOfMethodEvolutions(): Int = methodEvolutionMap.size
  def numberOfInvocationEvolutions(): Int = invocationEvolutionMap.size
  def numberOfDependencyEvolutions(): Int = dependencyEvolutionMap.size

  def dependenciesAt(release: String): Iterable[DependencyIdentifier] = {
    if(!releaseList.contains(release))
      throw new RuntimeException(s"Unknown release $release")

    dependencyEvolutions()
      .filter(_.isActiveIn.contains(release))
      .map(_.identifier)
  }

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

  def applyNewRelease(callgraph: LibraryCallgraph,
                      dependencies: Set[DependencyIdentifier],
                      release: String): Unit = {
    if(releaseList.contains(release)){
      throw new RuntimeException(s"Release has already been applied to CallGraphEvolution: $release")
    }

    callgraph
      .instantiatedTypeNames
      .foreach{ typeName =>
        if(!instantiatedTypesMap.contains(typeName)){
          instantiatedTypesMap.put(typeName, new mutable.ListBuffer())
        }
        instantiatedTypesMap(typeName).append(release)
      }

    log.info(s"Processing new release $release for $libraryName")
    releaseList.append(release)

    dependencies.foreach(setDependencyActiveInRelease(_, release))

    callgraph
      .allMethods
      .foreach { method =>

        setMethodActiveInRelease(method, release)

        callgraph
          .calleesOf(method)
          .foreach { callee =>
            setMethodActiveInRelease(callee, release)

            val ident = new MethodInvocationIdentifier(method.identifier, callee.identifier)
            setInvocationActiveInRelease(ident, release)
          }
      }
  }

  private def setDependencyActiveInRelease(identifier: DependencyIdentifier, release: String): Unit = {
    if(!dependencyEvolutionMap.contains(identifier)){
      dependencyEvolutionMap.put(identifier, new DependencyEvolution(identifier))
    }

    dependencyEvolutionMap(identifier).addActiveRelease(release)
  }

  private def setInvocationActiveInRelease(identifier: MethodInvocationIdentifier, release: String): Unit = {
    if(!invocationEvolutionMap.contains(identifier)){
      invocationEvolutionMap.put(identifier, new MethodInvocationEvolution(identifier))
    }

    invocationEvolutionMap(identifier).addActiveRelease(release)
  }

  private def setMethodActiveInRelease(method: LibraryMethod, release: String): Unit = {
    if(!methodEvolutionMap.contains(method.identifier)){
      methodEvolutionMap.put(method.identifier, new MethodEvolution(method.identifier))
    }

    val theEvolution = methodEvolutionMap(method.identifier)

    theEvolution.addActiveRelease(release)

    method.obligations.foreach(theEvolution.setObligationActiveIn(_, release))
  }

}



