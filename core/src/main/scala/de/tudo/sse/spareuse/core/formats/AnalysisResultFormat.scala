package de.tudo.sse.spareuse.core.formats
import spray.json.{JsArray, JsObject, JsValue}

import scala.util.Try

trait AnalysisResultFormat extends AnyValueFormat

case class ListResultFormat(elementFormat: AnyValueFormat,
                            elementExplanation: String = "") extends AnalysisResultFormat {

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case arr: JsArray =>
      arr.elements.forall(elementFormat.isValid)
    case _ => false
  }

  override def formatDescription(): String = s"A List of elements that represent $elementExplanation. The elements are formatted as: \n\t${elementFormat.formatDescription()}"

}

case class MapResultFormat(keyFormat: BaseValueFormat,
                           valueFormat: AnyValueFormat,
                           keyExplanation: String = "",
                           valueExplanation: String = "") extends AnalysisResultFormat {

  private def isValidKey(key: String): Boolean = keyFormat match {
    case StringFormat | EntityReferenceFormat => true
    case NumberFormat => Try(key.toInt).isSuccess
    case EmptyFormat => true
  }

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {
    case obj: JsObject =>
      obj.fields.forall( tuple => isValidKey(tuple._1) && valueFormat.isValid(tuple._2))

    case _ => false
  }

  override def formatDescription(): String =
    s"A Map of keys - representing $keyExplanation - to values representing $valueExplanation. Keys are formatted as ${keyFormat.formatDescription()}, values are formatted as: \n\t${valueFormat.formatDescription()}"

}

case class GraphResultFormat(edgePropertyFormat: Set[NamedPropertyFormat],
                             nodePropertyFormat: Set[NamedPropertyFormat],
                             nodeExplanation: String = "",
                             edgeExplanation: String = "") extends AnalysisResultFormat {

  // Represent a graph as an object {"nodes": [ { "__uid__": <id> , ...}], "edges": [ { "__from__": <id>, "__to__": <id>, ...} ]}
  private val nodeObjRep = ObjectResultFormat( Set(NamedPropertyFormat("__uid__", NumberFormat, "Unique node id")) ++ nodePropertyFormat)
  private val edgeObjRep = ObjectResultFormat( Set(NamedPropertyFormat("__from__", NumberFormat, "Source node id"), NamedPropertyFormat("__to__", NumberFormat, "Target node id")) ++ edgePropertyFormat)

  private val internalRep = toObjectFormat

  def nodeObjectFormat: ObjectResultFormat = nodeObjRep

  def edgeObjectFormat: ObjectResultFormat = edgeObjRep

  def toObjectFormat: ObjectResultFormat = ObjectResultFormat(Set(NamedPropertyFormat("nodes", ListResultFormat(nodeObjRep), nodeExplanation), NamedPropertyFormat("edges", ListResultFormat(edgeObjRep), edgeExplanation)))
  override def isValid(jsonValue: JsValue): Boolean = internalRep.isValid(jsonValue)

  override def formatDescription(): String = {
    val nodePropString = nodePropertyFormat.map(_.formatDescription()).mkString("\n\t")
    val edgePropString = edgePropertyFormat.map(_.formatDescription()).mkString("\n\t")
    s"A graph where nodes have ${nodePropertyFormat.size} different properties: \n\t$nodePropString\nGraph edges have ${edgePropertyFormat.size} different properties: \n\t$edgePropString"
  }

}

case class ObjectResultFormat(propertyFormats:Set[NamedPropertyFormat]) extends AnalysisResultFormat {

  private val propMap: Map[String, AnyValueFormat] = propertyFormats.map(p => (p.propertyName, p.propertyFormat)).toMap

  override def isValid(jsonValue: JsValue): Boolean = jsonValue match {

    case obj: JsObject =>
      // For every required format check that the value is present and valid
      propMap.forall { case (propName, propFormat) =>
        obj.fields.contains(propName) && propFormat.isValid(obj.fields(propName))
      }

    case _ => false
  }

  override def formatDescription(): String = {
    val propString = propertyFormats.map(_.formatDescription()).mkString("\n\t")

    s"An object containing ${propertyFormats.size} different properties: \n\t$propString"
  }
}
