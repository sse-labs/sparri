package org.tud.reachablemethods.analysis.impl

import akka.actor.ActorSystem
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.cg.{ApplicationWithoutJREEntryPointsFinder, InitialEntryPointsKey}
import org.opalj.br.instructions.INVOKESPECIAL
import org.tud.reachablemethods.analysis.dataaccess.{DataAccessor, InvocationDataAccessor, MethodDataAccessor}
import org.tud.reachablemethods.analysis.impl.callgraphs.CompositionalCallGraphBuilder
import org.tud.reachablemethods.analysis.{Configuration, ReachabilityAnalysis}
import org.tud.reachablemethods.analysis.model.ClassList.ClassList
import org.tud.reachablemethods.analysis.model.MavenIdentifier
import org.tud.reachablemethods.analysis.opal.OPALProjectHelper

import java.net.URL
import scala.util.{Failure, Success, Try}

class CompositionalReachabilityAnalysis(configuration: Configuration)(implicit system: ActorSystem) extends ReachabilityAnalysis {

  private[impl] val methodAccessor: MethodDataAccessor = new MethodDataAccessor(configuration)
  private[impl] val invocationAccessor: InvocationDataAccessor = new InvocationDataAccessor(configuration)
  private[impl] def dataAccessors: List[DataAccessor] = List(methodAccessor, invocationAccessor)

  dataAccessors.foreach(_.initialize())

  override def analyzeProject(projectClasses: ClassList, dependencyClasses: ClassList,
                              classFqnDependencyLookup: Map[String, MavenIdentifier], treatProjectAsLibrary: Boolean): Try[Any] = {

    val jreIdent = MavenIdentifier("<none>", "<jre>", methodAccessor.getIndexedJreVersion.get)
    val classFqnLookupWithJRE = classFqnDependencyLookup ++ OPALProjectHelper.jreClasses.map(c => (c._1.fqn, jreIdent)).toMap
    val allDependencies = classFqnLookupWithJRE.values.toList.distinct

    if(!analysisPossible(allDependencies)){
      Failure(new IllegalStateException("Cannot perform reachability analysis: requirements not satisfied"))
    } else {
      log.info("Initializing OPAL analysis infrastructure..")
      val opalProject = OPALProjectHelper.buildOPALProject(projectClasses, dependencyClasses, treatProjectAsLibrary)
      log.info("Done Initializing OPAL.")

      val analysisContext = new CompositionalAnalysisContext(classFqnLookupWithJRE, methodAccessor, invocationAccessor)

      // Add all instantiated types of current project to index
      analysisContext.indexInstantiatedTypes(getInstantiatedTypeNames(opalProject))

      // Add all instantiated types of dependency projects to index
      allDependencies.foreach { dependency =>
        analysisContext.indexInstantiatedTypes(methodAccessor.getArtifactMetadata(dependency.libraryIdentifier,
          dependency.version).map(_.instantiatedTypes).get) //TODO: Error Handling
      }

      val cgBuilder = new CompositionalCallGraphBuilder(opalProject, analysisContext)

      cgBuilder.buildCallGraph()


      dataAccessors.foreach(_.shutdown())

      Success()
    }

  }

  override def analysisPossible(dependencies: Iterable[MavenIdentifier]): Boolean = {
    dependencies.forall(dep => methodAccessor.libraryInIndex(dep.libraryIdentifier))
  }

  private[impl] def getInstantiatedTypeNames(project: Project[URL]): Set[String] = {
    project
      .allMethods
      .filter(m => project.isProjectType(m.classFile.thisType) && m.body.isDefined)
      .flatMap(m => m.body.get.instructions)
      .filter(i => i != null && i.isMethodInvocationInstruction && i.isInstanceOf[INVOKESPECIAL])
      .map(i => i.asInstanceOf[INVOKESPECIAL])
      .filter(i => i.name.equals("<init>"))
      .map(_.declaringClass.fqn)
      .toSet
  }
}
