package de.tudo.sse.spareuse.core.formats.json

import de.tudo.sse.spareuse.core.formats
import de.tudo.sse.spareuse.core.formats.AnyValueFormat
import spray.json.{JsArray, JsObject, JsString, JsValue, JsonWriter}

import scala.language.implicitConversions

object CustomFormatWriter extends JsonWriter[AnyValueFormat] {


  override def write(format: AnyValueFormat): JsValue = {

    implicit def jsonify(s: String): JsValue = JsString(s)

    format match {
      case formats.StringFormat => JsObject(("Type", JsString("StringFormat")))
      case formats.NumberFormat => JsObject(("Type", JsString("NumberFormat")))
      case formats.EmptyFormat => JsObject(("Type", JsString("EmptyFormat")))
      case formats.EntityReferenceFormat => JsObject(("Type", JsString("EntityReferenceFormat")))

      case formats.ListResultFormat(elementFormat, explanation) =>
        JsObject(("Type", "ListResultFormat"), ("ElementDescription", explanation), ("ElementFormat", write(elementFormat)))

      case formats.MapResultFormat(keyFormat, valueFormat, keyDescription, valueDescription) =>
        JsObject(("Type", "MapResultFormat"), ("KeyDescription", keyDescription), ("ValueDescription", valueDescription),
          ("KeyFormat", write(keyFormat)), ("ValueFormat", write(valueFormat)))

      case formats.GraphResultFormat(edgeProperties, nodeProperties, nodeDescription, edgeDescription) =>
        JsObject(("Type", "GraphResultFormat"), ("NodeDescription", nodeDescription), ("EdgeDescriptions", edgeDescription),
          ("NodeProperties", JsArray(nodeProperties.map(write).toVector)), ("EdgeProperties", JsArray(edgeProperties.map(write).toVector)))

      case formats.ObjectResultFormat(properties) =>
        val allProps = List(("Type", JsString("ObjectResultFormat"))) ++ properties.flatMap(p => Seq((p.propertyName, write(p.propertyFormat)), (p.propertyName + "Description", JsString(p.explanation)))).toList
        JsObject(allProps :_*)
    }
  }

}
