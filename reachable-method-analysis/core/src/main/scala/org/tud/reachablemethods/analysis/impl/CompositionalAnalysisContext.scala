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

  private val instantiatedTypeNames: mutable.ListBuffer[String] = new mutable.ListBuffer[String]()

  private val elasticIdMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()
  private val signatureMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()

  private val fqnTypeIndex: mutable.Map[String, ObjectType] = new mutable.HashMap()

  private val uidObligationIndex: mutable.Map[String, Option[Iterable[Method]]] = new mutable.HashMap()


  log.debug("Indexing all types by FQN...")
  buildFqnTypeIndex()
  log.debug("Done indexing all types.")
  log.debug("Indexing all dependency methods...")
  loadAllDependencies()
  log.debug("Done indexing all dependencies.")


  def classHierarchy: ClassHierarchy = opalProject.classHierarchy

  def instantiatedTypes: List[String] = instantiatedTypeNames.toList

  def indexInstantiatedTypes(typeNames: Iterable[String]): Unit = {
    instantiatedTypeNames.appendAll(typeNames)
  }

  def isTypeInstantiated(typeName: String): Boolean = instantiatedTypes.contains(typeName)

  def resolveObligationInLibrary(obligation: InvocationObligation, libraryIdent: String): Option[Iterable[Method]] = {
    val obligationKey = obligationInLibraryUid(obligation, libraryIdent)

    if(uidObligationIndex.contains(obligationKey)){
      uidObligationIndex(obligationKey)
    } else {
      val result = fqnTypeIndex
        .get(obligation.declaredTypeName)
        .map{ typeObj =>
          classHierarchy.allSubtypes(typeObj, reflexive = false)
            .filter(t => isTypeInstantiated(t.fqn))
            .filter(t => opalProject.isProjectType(t)) //TODO: Not true, need to find subtypes in other libraries as well..all libraries except current one!
            .flatMap(t => opalProject.allMethods.filter(m => m.classFile.thisType.equals(t))) //TODO: Use another index for this
            .filter(m => (m.name + m.descriptor.valueToString).equals(obligation.methodDescription))
        }

      uidObligationIndex.put(obligationKey, result)

      result
    }
  }


  def getMethodBySignatureAndClass(signature: String, classFqn: String): Option[ElasticMethodData] = {
    if(signatureMethodIndex.contains(signature)){
      Some(signatureMethodIndex(signature))
    } else {
      classFileFqnDependencyMap
        .get(classFqn)
        .flatMap(ident => methodDataAccessor.getArtifactMethodBySignatures(List(signature), ident.libraryIdentifier, ident.version).toOption)
        .flatMap(hits => hits.map(h => addToMethodIndices(h)).headOption)
    }
  }

  def signatureLookup(signature: String): Option[ElasticMethodData] = {
    signatureMethodIndex.get(signature)
  }

  private def addToMethodIndices(methodData: ElasticMethodData, preloadCallees: Boolean = true): ElasticMethodData = {

    elasticIdMethodIndex.put(methodData.elasticId, methodData)
    signatureMethodIndex.put(methodData.signature, methodData)

    if(preloadCallees){
      indexCallees(methodData)
    }

    methodData
  }

  private def indexCallees(methodData: ElasticMethodData): Unit = {

    val missingSignatures = methodData.calleeSignatures.filter(!signatureMethodIndex.contains(_))

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

  private def loadJreData(): Unit = {
    val jreIdent = classFileFqnDependencyMap.values.find(i => i.artifactId.equals("<jre>")).get
    methodDataAccessor.getArtifactMethods(jreIdent.libraryIdentifier, jreIdent.version) match {
      case Success(hits) =>
        hits.foreach(addToMethodIndices(_, preloadCallees = false))
      case Failure(ex) =>
        log.error("Failed to load JRE methods", ex)
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
