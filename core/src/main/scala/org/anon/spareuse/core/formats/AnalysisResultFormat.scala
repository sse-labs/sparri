package org.anon.spareuse.core.formats

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

  override def formatDescription(indent: Int = 0): String = {
    val currIndent = "\t"*indent
    s"A List of elements that represent $elementExplanation. The elements are formatted as: \n$currIndent\t${elementFormat.formatDescription(indent + 1)}"
  }

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

  override def formatDescription(indent: Int = 0): String = {
    val currIndent = "\t"*indent
    s"A Map of keys - representing $keyExplanation - to values representing $valueExplanation. Keys are formatted as ${keyFormat.formatDescription(indent + 1)}, values are formatted as: \n$currIndent\t${valueFormat.formatDescription(indent + 1)}"
  }

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

  override def formatDescription(indent: Int = 0): String = {
    val currIndent = "\t"*indent
    val nodePropString = nodePropertyFormat.map(_.formatDescription(indent + 1)).mkString(s"\n$currIndent\t")
    val edgePropString = edgePropertyFormat.map(_.formatDescription(indent + 1)).mkString(s"\n$currIndent\t")
    s"A graph where nodes have ${nodePropertyFormat.size} different properties: \n$currIndent\t$nodePropString\n${currIndent}Graph edges have ${edgePropertyFormat.size} different properties: \n$currIndent\t$edgePropString"
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

  override def formatDescription(indent: Int = 0): String = {
    val currIndent = "\t"*indent
    val propString = propertyFormats.map(_.formatDescription(indent + 1)).mkString(s"\n$currIndent\t")

    s"An object containing ${propertyFormats.size} different properties: \n$currIndent\t$propString"
  }
}

object ObjectResultFormat {
  def apply(propertyFormats: NamedPropertyFormat*): ObjectResultFormat = {
    ObjectResultFormat(propertyFormats.toSet)
  }
}
