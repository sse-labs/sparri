package org.tud.cgcrawling.model

import de.tudo.classfilefeatures.common.maven.model.MavenDependencyIdentifier
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class LibraryCallGraphEvolution(val groupId: String, val artifactId: String) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val methodEvolutionMap: mutable.Map[MethodIdentifier, MethodEvolution] = new mutable.HashMap()
  private val invocationEvolutionMap: mutable.Map[MethodInvocationIdentifier, MethodInvocationEvolution] =
    new mutable.HashMap()

  private val invocationEvolutionPerMethod: mutable.Map[MethodIdentifier, mutable.Set[MethodInvocationEvolution]] =
    new mutable.HashMap()

  private val dependencyEvolutionMap: mutable.Map[MavenDependencyIdentifier, DependencyEvolution] = new mutable.HashMap()


  private val releaseSet: mutable.Set[String] = new mutable.HashSet[String]

  private val typeEvolutionMap: mutable.Map[String, TypeEvolution] = new mutable.HashMap()

  val libraryName = s"$groupId:$artifactId"

  def releases(): Set[String] = releaseSet.toSet
  def methodEvolutions(): Iterable[MethodEvolution] = methodEvolutionMap.values
  def methodInvocationEvolutions(): Iterable[MethodInvocationEvolution] = invocationEvolutionMap.values
  def dependencyEvolutions(): Iterable[DependencyEvolution] = dependencyEvolutionMap.values
  def typeEvolutions(): Iterable[TypeEvolution] = typeEvolutionMap.values

  def numberOfMethodEvolutions(): Int = methodEvolutionMap.size
  def numberOfInvocationEvolutions(): Int = invocationEvolutionMap.size
  def numberOfDependencyEvolutions(): Int = dependencyEvolutionMap.size
  def numberOfTypeEvolutions(): Int = typeEvolutionMap.size

  def dependenciesAt(release: String): Iterable[MavenDependencyIdentifier] = {
    if(!releaseSet.contains(release))
      throw new RuntimeException(s"Unknown release $release")

    dependencyEvolutions()
      .filter(_.isActiveIn.contains(release))
      .map(_.identifier)
  }

  def methodsAt(release: String): Iterable[MethodIdentifier] = {
    if(!releaseSet.contains(release))
      throw new RuntimeException(s"Unknown release $release")

    methodEvolutions()
      .filter(_.isActiveIn.contains(release))
      .map(_.identifier)
  }

  def calleeEvolutionsAt(caller: MethodIdentifier): Set[MethodInvocationEvolution] = {
    invocationEvolutionPerMethod.get(caller).map(_.toSet).getOrElse(Set.empty)
  }

  def calleesAt(caller: MethodIdentifier, release: String): Iterable[MethodIdentifier] ={
    if(!releaseSet.contains(release))
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
                      dependencies: Set[MavenDependencyIdentifier],
                      release: String): Unit = {
    if(releaseSet.contains(release)){
      throw new RuntimeException(s"Release has already been applied to CallGraphEvolution: $release")
    }

    log.info(s"Processing new release $release for $libraryName")
    releaseSet.add(release)

    callgraph.hierarchy.allTypeNames.foreach{ typeFqn =>
      if(!typeEvolutionMap.contains(typeFqn)){
        typeEvolutionMap.put(typeFqn, new TypeEvolution(typeFqn))
      }

      typeEvolutionMap(typeFqn).addActiveRelease(release)
      typeEvolutionMap(typeFqn).setParentTypeIn(release, callgraph.hierarchy.parentTypeOf(typeFqn))
      typeEvolutionMap(typeFqn).setParentInterfacesIn(release, callgraph.hierarchy.parentInterfacesOf(typeFqn))

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

  private def setDependencyActiveInRelease(identifier: MavenDependencyIdentifier, release: String): Unit = {
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



