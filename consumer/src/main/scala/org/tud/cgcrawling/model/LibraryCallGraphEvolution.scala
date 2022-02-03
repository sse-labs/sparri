package org.tud.cgcrawling.model

import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class LibraryCallGraphEvolution(val groupId: String, val artifactId: String) {

  type InstantiatedTypeEvolution = (String, List[String])

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val methodEvolutionMap: mutable.Map[MethodIdentifier, MethodEvolution] = new mutable.HashMap()
  private val invocationEvolutionMap: mutable.Map[MethodInvocationIdentifier, MethodInvocationEvolution] =
    new mutable.HashMap()

  private val invocationEvolutionPerMethod: mutable.Map[MethodIdentifier, mutable.Set[MethodInvocationEvolution]] =
    new mutable.HashMap()

  private val dependencyEvolutionMap: mutable.Map[DependencyIdentifier, DependencyEvolution] = new mutable.HashMap()


  private val releaseList: mutable.ListBuffer[String] = new ListBuffer[String]

  private val typeEvolutionMap: mutable.Map[String, TypeEvolution] = new mutable.HashMap()

  val libraryName = s"$groupId:$artifactId"

  def releases(): List[String] = releaseList.toList
  def methodEvolutions(): Iterable[MethodEvolution] = methodEvolutionMap.values
  def methodInvocationEvolutions(): Iterable[MethodInvocationEvolution] = invocationEvolutionMap.values
  def dependencyEvolutions(): Iterable[DependencyEvolution] = dependencyEvolutionMap.values
  def typeEvolutions(): Iterable[TypeEvolution] = typeEvolutionMap.values

  //TODO: Remove this legacy API
  def instantiatedTypeEvolutions(): Iterable[InstantiatedTypeEvolution] = typeEvolutionMap
    .values
    .filter(tEvo => tEvo.isInstantiatedIn.nonEmpty)
    .map(tEvo => {
      (tEvo.typeFqn, tEvo.isInstantiatedIn)
    })

  def numberOfMethodEvolutions(): Int = methodEvolutionMap.size
  def numberOfInvocationEvolutions(): Int = invocationEvolutionMap.size
  def numberOfDependencyEvolutions(): Int = dependencyEvolutionMap.size
  def numberOfTypeEvolutions(): Int = typeEvolutionMap.size

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

  def calleeEvolutionsAt(caller: MethodIdentifier): Set[MethodInvocationEvolution] = {
    invocationEvolutionPerMethod.get(caller).map(_.toSet).getOrElse(Set.empty)
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

    log.info(s"Processing new release $release for $libraryName")
    releaseList.append(release)

    callgraph.hierarchy.allTypeNames.foreach{ typeFqn =>
      if(!typeEvolutionMap.contains(typeFqn)){
        typeEvolutionMap.put(typeFqn, new TypeEvolution(typeFqn))
      }

      typeEvolutionMap(typeFqn).addActiveRelease(release)
      typeEvolutionMap(typeFqn).setChildrenIn(release, callgraph.hierarchy.childrenOf(typeFqn).getOrElse(Iterable.empty))

      if(callgraph.instantiatedTypeNames.contains(typeFqn)){
        typeEvolutionMap(typeFqn).setInstantiatedIn(release)
      }
    }

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

    if(!invocationEvolutionPerMethod.contains(identifier.callerIdent)){
      invocationEvolutionPerMethod.put(identifier.callerIdent, new mutable.HashSet[MethodInvocationEvolution]())
    }

    invocationEvolutionPerMethod(identifier.callerIdent).add(invocationEvolutionMap(identifier))
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



