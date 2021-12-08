package org.tud.reachablemethods.analysis.impl

import org.opalj.br.analyses.Project
import org.tud.reachablemethods.analysis.dataaccess.ElasticMethodData
import org.tud.reachablemethods.analysis.model.MavenIdentifier

import java.net.URL
import scala.collection.mutable

class CompositionalAnalysisContext(project: Project[URL],
                                  classFileFqnDependencyMap: Map[String, MavenIdentifier]){

  private val instantiatedTypeNames: mutable.ListBuffer[String] = new mutable.ListBuffer[String]()

  private val elasticIdMethodIndex: mutable.Map[String, ElasticMethodData] = new mutable.HashMap()
  private val libraryMethodIndex: mutable.Map[String, mutable.Map[String, ElasticMethodData]] = new mutable.HashMap()

  def instantiatedTypes: List[String] = instantiatedTypeNames.toList

  def indexMethods(methods: Iterable[ElasticMethodData]): Unit ={
    methods.foreach{ data =>
      elasticIdMethodIndex.put(data.elasticId, data)

      if(!libraryMethodIndex.contains(data.analyzedLibrary)){
        libraryMethodIndex.put(data.analyzedLibrary, new mutable.HashMap())
      }

      libraryMethodIndex(data.analyzedLibrary).put(data.signature, data)
    }
  }

  def indexInstantiatedTypes(typeNames: Iterable[String]): Unit = {
    instantiatedTypeNames.appendAll(typeNames)
  }

  def isTypeInstantiated(typeName: String): Boolean = instantiatedTypes.contains(typeName)

}
