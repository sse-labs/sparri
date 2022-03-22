package de.tudo.sse.classfilefeatures.extractor.model

import scala.collection.mutable

class ValueEvolution[T] {

  private val valueEvolutionMap: mutable.Map[T, mutable.Set[String]] = new mutable.HashMap

  def hasValue(value: T): Boolean = valueEvolutionMap.contains(value)

  def addValueAt(release: String, value: T): Unit = {
    if(!hasValue(value)){
      valueEvolutionMap.put(value, mutable.HashSet.empty)
    }

    valueEvolutionMap(value).add(release)
  }

  def hasSingleValue: Boolean = valueEvolutionMap.size == 1
  def asSingleValue: Option[T] = if (hasSingleValue) valueEvolutionMap.keys.headOption else None

  def getDefaultValueOpt: Option[T] = if(valueEvolutionMap.isEmpty) None else Some(getDefaultValue)

  def getDefaultValue: T = {
    assert(valueEvolutionMap.nonEmpty)

    if(hasSingleValue){
      asSingleValue.get
    } else {
      var mostReleasesPerValue = -1
      var defaultValue: Option[T] = None

      valueToReleasesMap.foreach { tuple =>
        val value = tuple._1
        val noOfReleases = tuple._2.size

        if(noOfReleases > mostReleasesPerValue){
          mostReleasesPerValue = noOfReleases
          defaultValue = Some(value)
        }
      }

      assert(defaultValue.isDefined)
      defaultValue.get
    }
  }

  def valueToReleasesMap: Map[T, Set[String]] = valueEvolutionMap.mapValues(_.toSet).toMap

  def getReleaseToValueTuples: Seq[(String, T)] = valueEvolutionMap.flatMap(t => t._2.map(r => (r, t._1)).toSeq).toSeq.distinct
}
