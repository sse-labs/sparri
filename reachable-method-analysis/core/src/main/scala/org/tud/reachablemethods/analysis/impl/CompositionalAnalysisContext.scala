package org.tud.reachablemethods.analysis.impl

import org.opalj.br.{Method, ObjectType}
import org.opalj.br.analyses.Project
import org.tud.reachablemethods.analysis.dataaccess.{ArtifactMetadata, ElasticMethodData, InvocationObligation, MethodDataAccessor}
import org.tud.reachablemethods.analysis.impl.callgraphs.CallGraphBuilder
import org.tud.reachablemethods.analysis.logging.{AnalysisLogger, AnalysisLogging}
import org.tud.reachablemethods.analysis.model.{MavenIdentifier, TypeHierarchy}

import java.net.URL
import scala.collection.mutable
import scala.util.{Failure, Success}

class CompositionalAnalysisContext(dependencies: Iterable[MavenIdentifier],
                                   methodDataAccessor: MethodDataAccessor,
                                   opalProject: Project[URL],
                                   override val log: AnalysisLogger) extends AnalysisLogging {

  // The type hierarchy that will be extended while processing new type information
  private implicit val typeHierarchy: TypeHierarchy = new TypeHierarchy(log)

  // String sets keeping track of instantiated types and methods seen
  private val instantiatedTypeNames: mutable.Set[String] = new mutable.HashSet[String]()
  private val methodSignaturesProcessed: mutable.Set[String] = new mutable.HashSet()

  // Lookup maps to speed up analysis performance. Will be extended while new information is loaded
  private val projectOnly_ObjectTypeFqnLookup: mutable.Map[String, ObjectType] = new mutable.HashMap()
  private val methodInformationSignatureLookup: mutable.Map[String, Either[Method, ElasticMethodData]] = new mutable.HashMap()
  private val methodInformationTypeFqnLookup: mutable.Map[String, mutable.Set[Either[Method, ElasticMethodData]]] = new mutable.HashMap()



  private val uidObligationCache: mutable.Map[String, Option[Iterable[Either[Method, ElasticMethodData]]]] = new mutable.HashMap()


  //---------------------------------------------------
  //  START BUILDING LOOKUPS
  //---------------------------------------------------

  log.debug("Building type hierarchy index...")
  loadAllDependencyTypes()
  loadAllProjectTypes()
  log.debug("Building method indices...")
  // Important to first load dependency methods. This way, we only add project information about methods that are not
  // in elastic
  loadAllDependencyMethods()
  loadAllProjectMethods()
  log.debug("Done building indices.")

  //---------------------------------------------------
  //  END BUILDING LOOKUPS
  //
  //  At this point, all indices are in their final form and
  //  could be materialized into lazy vals for querying
  //---------------------------------------------------


  // Indices that materialize into immutable collections once querying starts
  lazy val instantiatedTypes: Set[String] = instantiatedTypeNames.toSet


  def isTypeInstantiated(typeName: String): Boolean = instantiatedTypes.contains(typeName)

  def addMethodSeen(methodSignature: String): Unit = methodSignaturesProcessed.add(methodSignature)

  def methodSeen(methodSignature: String): Boolean = methodSignaturesProcessed.contains(methodSignature)

  def numberOfMethodsSeen(): Int = methodSignaturesProcessed.size

  def methodSignaturesSeen: Set[String]= methodSignaturesProcessed.toSet

  def obligationResolved(obligation: InvocationObligation, libraryIdent: String): Boolean = {
    uidObligationCache.contains(obligationInLibraryUid(obligation, libraryIdent))
  }

  //TODO: Redesign using new type hierarchy
  /*def resolveObligationInLibrary(obligation: InvocationObligation, libraryIdent: String): Option[Iterable[Either[Method, ElasticMethodData]]]= {
    val obligationKey = obligationInLibraryUid(obligation, libraryIdent)

    if(uidObligationCache.contains(obligationKey)){
      uidObligationCache(obligationKey)
    } else {

      val declTypeOpt = typeIndex.get(obligation.declaredTypeName)

      val result = declTypeOpt.map{ declType =>

        classHierarchy.allSubtypesIterator(declType, reflexive = true)
          .filter(t => instantiatedTypes.contains(t.fqn))
          .flatMap(t => methodObjectIndex.getOrElse(t.fqn, Iterable.empty))
          .filter(m => (m.name + m.descriptor.valueToString).equals(obligation.methodDescription))
          .map(m => if(methodDataIndex.contains(m.fullyQualifiedSignature)) Right(methodDataIndex(m.fullyQualifiedSignature)) else Left(m))
          .toSet
      }

      uidObligationCache.put(obligationKey, result)

      result
    }
  }*/

  def signatureLookup(signature: String): Option[Either[Method, ElasticMethodData]] = {
    methodInformationSignatureLookup.get(signature)
  }

  private def obligationInLibraryUid(obligation: InvocationObligation, libIdent: String): String =
    obligation.declaredTypeName + obligation.methodDescription + libIdent



  //---------------------------------------------------------
  //  START OF INDEX CREATION METHODS
  //---------------------------------------------------------

  private def addToMethodIndices(methodData: ElasticMethodData): Unit = {

    if(!methodData.isExtern){
      methodInformationSignatureLookup.put(methodData.signature, Right(methodData))
    }

    if(!methodInformationTypeFqnLookup.contains(methodData.typeFqn)){
      methodInformationTypeFqnLookup.put(methodData.typeFqn, mutable.Set.empty)
    }

    methodInformationTypeFqnLookup(methodData.typeFqn).add(Right(methodData))
  }

  private def addToMethodIndices(method: Method): Unit = {
    if(!methodInformationSignatureLookup.contains(method.fullyQualifiedSignature)){
      methodInformationSignatureLookup.put(method.fullyQualifiedSignature, Left(method))
    }

    val typeFqn = method.classFile.thisType.fqn

    if(!methodInformationTypeFqnLookup.contains(typeFqn)){
      methodInformationTypeFqnLookup.put(typeFqn, mutable.Set.empty)
    }

    methodInformationTypeFqnLookup(typeFqn).add(Left(method))
  }

  private def addToTypeIndices(metadata: ArtifactMetadata): Unit = {
    metadata.types.foreach { artifactType =>
      if(artifactType.isInstantiated) instantiatedTypeNames.add(artifactType.fqn)

      typeHierarchy.addType(artifactType.fqn, artifactType.parentType, artifactType.parentInterfaces)

      if(!methodInformationTypeFqnLookup.contains(artifactType.fqn)){
        methodInformationTypeFqnLookup.put(artifactType.fqn, mutable.Set.empty)
      }
    }
  }

  private def loadAllDependencyMethods(): Unit = {
    dependencies.foreach { ident =>
      methodDataAccessor.getArtifactMethods(ident.libraryIdentifier, ident.version) match {
        case Success(hits) =>
          hits.foreach(addToMethodIndices)
        case Failure(ex) =>
          log.error(s"Failed to load Dependency methods for ${ident.libraryIdentifier}", ex)
      }
    }
  }

  private def loadAllProjectMethods(): Unit = {
    opalProject.allMethods.foreach(addToMethodIndices)
  }

  private def loadAllDependencyTypes(): Unit = {
    dependencies.foreach { ident =>
      methodDataAccessor.getArtifactMetadata(ident.libraryIdentifier, ident.version) match {
        case Success(metadata) =>
          addToTypeIndices(metadata)
        case Failure(ex) =>
          log.error(s"Failed to load Dependency metadata for ${ident.libraryIdentifier}", ex)
      }
    }
  }

  private def loadAllProjectTypes(): Unit = {
    val opalHierarchy = opalProject.classHierarchy

    opalHierarchy.foreachKnownType { objectType =>
      val typeFqn = objectType.fqn
      val parentOpt = opalHierarchy.superclassType(objectType).map(_.fqn)
      //TODO: Semantics of superinterfaceTypes correct?
      val interfaces = opalHierarchy.superinterfaceTypes(objectType).map(set => set.map(_.fqn)).getOrElse(Iterable.empty)

      typeHierarchy.addType(typeFqn, parentOpt, interfaces)
      projectOnly_ObjectTypeFqnLookup.put(typeFqn, objectType)

      if(!methodInformationTypeFqnLookup.contains(typeFqn)){
        methodInformationTypeFqnLookup.put(typeFqn, mutable.Set.empty)
      }
    }

    CallGraphBuilder
      .getInstantiatedTypeNames(opalProject, projectOnly = false)
      .foreach(instantiatedTypeNames.add)
  }

}
