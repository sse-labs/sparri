package org.tud.reachablemethods.analysis.impl

import org.slf4j.{Logger, LoggerFactory}
import org.tud.reachablemethods.analysis.dataaccess.{ElasticMethodData, InvocationDataAccessor, MethodDataAccessor}
import org.tud.reachablemethods.analysis.model.MavenIdentifier

import scala.collection.mutable
import scala.util.Failure

class CompositionalAnalysisContext(classFileFqnDependencyMap: Map[String, MavenIdentifier],
                                   methodDataAccessor: MethodDataAccessor, invocationDataAccessor: InvocationDataAccessor){

  private val log: Logger = LoggerFactory.getLogger(getClass)

  private val instantiatedTypeNames: mutable.ListBuffer[String] = new mutable.ListBuffer[String]()

  private val elasticIdMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()
  private val signatureMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()

  def instantiatedTypes: List[String] = instantiatedTypeNames.toList

  def indexInstantiatedTypes(typeNames: Iterable[String]): Unit = {
    instantiatedTypeNames.appendAll(typeNames)
  }

  def isTypeInstantiated(typeName: String): Boolean = instantiatedTypes.contains(typeName)

  def getMethodBySignatureAndClass(signature: String, classFqn: String): Option[ElasticMethodData] = {
    if(signatureMethodIndex.contains(signature)){
      Some(signatureMethodIndex(signature))
    } else {
      classFileFqnDependencyMap
        .get(classFqn)
        .flatMap(ident => methodDataAccessor.getArtifactMethodBySignature(signature, ident.libraryIdentifier, ident.version).toOption)
        .map(addToMethodIndices)
    }
  }



  def getInvokedMethods(elasticId: String, version: String): Seq[ElasticMethodData] = {
    val calleeIds = invocationDataAccessor.getCalleesForMethod(elasticId, version)
    indexMethodsById(calleeIds, version)

    calleeIds.map(elasticIdMethodIndex(_))
  }

  private def indexMethodsById(elasticIds: Seq[String], version: String): Unit = {

    val missingInIndex = elasticIds.filter(!elasticIdMethodIndex.contains(_))

    if(missingInIndex.nonEmpty){
      methodDataAccessor
        .getArtifactMethodByElasticIds(missingInIndex, version)
        .map( methods =>
          methods.foreach(addToMethodIndices)
        ) match {
        case Failure(ex) =>
          log.error("Failed to index methods by id", ex)
        case _ =>
      }
    }
  }


  private def addToMethodIndices(methodData: ElasticMethodData): ElasticMethodData = {
    elasticIdMethodIndex.put(methodData.elasticId, methodData)
    signatureMethodIndex.put(methodData.signature, methodData)
    methodData
  }

}
