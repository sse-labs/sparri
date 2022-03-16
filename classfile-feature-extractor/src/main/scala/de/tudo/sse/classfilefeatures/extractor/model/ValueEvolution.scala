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

  def valueToReleasesMap: Map[T, Set[String]] = valueEvolutionMap.mapValues(_.toSet).toMap
}
