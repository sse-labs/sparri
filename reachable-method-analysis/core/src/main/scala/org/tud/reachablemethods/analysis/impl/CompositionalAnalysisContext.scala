package org.tud.reachablemethods.analysis.impl

import org.opalj.br.{ClassHierarchy, Method, ObjectType}
import org.opalj.br.analyses.Project
import org.slf4j.{Logger, LoggerFactory}
import org.tud.reachablemethods.analysis.dataaccess.{ElasticMethodData, InvocationObligation, MethodDataAccessor}
import org.tud.reachablemethods.analysis.model.MavenIdentifier

import java.net.URL
import scala.collection.mutable
import scala.util.{Failure, Success}

class CompositionalAnalysisContext(classFileFqnDependencyMap: Map[String, MavenIdentifier],
                                   methodDataAccessor: MethodDataAccessor, opalProject: Project[URL]){

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val instantiatedTypeNames: mutable.Set[String] = new mutable.HashSet[String]()

  private val elasticIdMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()
  private val signatureMethodDataIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()
  private val fqnTypeIndex: mutable.Map[String, ObjectType] = new mutable.HashMap()
  private val typeFqnMethodObjectIndex: mutable.Map[String, mutable.Set[Method]] = new mutable.HashMap()


  private val uidObligationCache: mutable.Map[String, Option[Iterable[Either[Method, ElasticMethodData]]]] = new mutable.HashMap()

  private val methodSignaturesProcessed: mutable.Set[String] = new mutable.HashSet()


  log.debug("Indexing all types by FQN...")
  buildFqnTypeIndex()
  buildSignatureMethodIndex()
  log.debug("Done indexing all types.")
  log.debug("Indexing all dependency methods...")
  loadAllDependencies()
  log.debug("Done indexing all dependencies.")


  // Indices that materialize into immutable collections once querying starts
  lazy val classHierarchy: ClassHierarchy = opalProject.classHierarchy

  lazy val instantiatedTypes: Set[String] = instantiatedTypeNames.toSet

  lazy val typeIndex: Map[String, ObjectType] = fqnTypeIndex.toMap

  lazy val methodObjectIndex: Map[String, Set[Method]] = typeFqnMethodObjectIndex.mapValues(_.toSet).toMap

  lazy val methodDataIndex: Map[String, ElasticMethodData] = signatureMethodDataIndex.toMap

  def indexInstantiatedTypes(typeNames: Iterable[String]): Unit = {
    typeNames.foreach(instantiatedTypeNames.add)
  }

  def isTypeInstantiated(typeName: String): Boolean = instantiatedTypes.contains(typeName)

  def addMethodSeen(methodSignature: String): Unit = methodSignaturesProcessed.add(methodSignature)

  def methodSeen(methodSignature: String): Boolean = methodSignaturesProcessed.contains(methodSignature)

  def numberOfMethodsSeen(): Int = methodSignaturesProcessed.size

  def resolveObligationInLibrary(obligation: InvocationObligation, libraryIdent: String): Option[Iterable[Either[Method, ElasticMethodData]]]= {
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
  }


  def getMethodBySignatureAndClass(signature: String, classFqn: String): Option[ElasticMethodData] = {
    if(signatureMethodDataIndex.contains(signature)){
      Some(signatureMethodDataIndex(signature))
    } else {
      classFileFqnDependencyMap
        .get(classFqn)
        .flatMap(ident => methodDataAccessor.getArtifactMethodBySignatures(List(signature), ident.libraryIdentifier, ident.version).toOption)
        .flatMap(hits => hits.map(h => addToMethodIndices(h, false)).headOption)
    }
  }

  def signatureLookup(signature: String): Option[ElasticMethodData] = {
    signatureMethodDataIndex.get(signature)
  }

  private def addToMethodIndices(methodData: ElasticMethodData, preloadCallees: Boolean = true): ElasticMethodData = {

    elasticIdMethodIndex.put(methodData.elasticId, methodData)

    if(!methodData.isExtern){
      signatureMethodDataIndex.put(methodData.signature, methodData)
    }

    if(preloadCallees){
      indexCallees(methodData)
    }

    methodData
  }

  private def indexCallees(methodData: ElasticMethodData): Unit = {

    val missingSignatures = methodData.calleeSignatures.filter(!signatureMethodDataIndex.contains(_))

    if(missingSignatures.nonEmpty){
      methodDataAccessor
        .getArtifactMethodBySignatures(missingSignatures, methodData.analyzedLibrary, methodData.libraryVersion) match {
        case Success(callees) =>
          callees.foreach(c => addToMethodIndices(c, preloadCallees = false)) // For now: No transitive preload
        case Failure(ex) =>
          log.error(s"Failed to index callees of ${methodData.signature}", ex)
      }
    }
  }

  private def obligationInLibraryUid(obligation: InvocationObligation, libIdent: String): String =
    obligation.declaredTypeName + obligation.methodDescription + libIdent

  private def buildFqnTypeIndex(): Unit = {
    opalProject.allClassFiles.foreach(cf => fqnTypeIndex.put(cf.thisType.fqn, cf.thisType))
  }

  private def buildSignatureMethodIndex(): Unit = {
    opalProject.allMethods.foreach { m =>

      if(!typeFqnMethodObjectIndex.contains(m.classFile.thisType.fqn)){
        typeFqnMethodObjectIndex.put(m.classFile.thisType.fqn, new mutable.HashSet[Method]())
      }

      typeFqnMethodObjectIndex(m.classFile.thisType.fqn).add(m)
    }
  }

  private def loadAllDependencies(): Unit = {
    classFileFqnDependencyMap.values.toList.distinct.foreach{ ident =>
      methodDataAccessor.getArtifactMethods(ident.libraryIdentifier, ident.version) match {
        case Success(hits) =>
          hits.foreach(addToMethodIndices(_, preloadCallees = false))
        case Failure(ex) =>
          log.error(s"Failed to load Dependency methods for ${ident.libraryIdentifier}", ex)
      }
    }
  }

}
