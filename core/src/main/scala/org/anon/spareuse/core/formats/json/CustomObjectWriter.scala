package org.anon.spareuse.core.formats.json

import org.anon.spareuse.core.formats
import org.anon.spareuse.core.formats.{AnyValueFormat, BaseValueFormat, ListResultFormat}
import org.anon.spareuse.core.model.analysis.GraphResult
import org.anon.spareuse.core.model.entities.JavaEntities.PathIdentifiableJavaEntity
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import spray.json.{DeserializationException, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, JsonWriter, enrichAny}

import scala.reflect.runtime.universe.runtimeMirror

class CustomObjectWriter(format: AnyValueFormat) extends JsonWriter[Object] {

  override def write(obj: Object): JsValue = format match {
    case formats.StringFormat => JsString(obj.toString)
    case formats.NumberFormat => JsNumber(obj.toString)
    case formats.EmptyFormat => JsString("<EMPTY>")
    case formats.EntityReferenceFormat if obj.isInstanceOf[PathIdentifiableJavaEntity] =>
      val entityLike = obj.asInstanceOf[PathIdentifiableJavaEntity]
      JsString(entityLike.uid)
    case formats.EntityReferenceFormat =>
      JsString(obj.toString)

    case formats.ListResultFormat(elemFormat, _) if obj.isInstanceOf[Seq[Any]] =>

      val elemWriter = new CustomAnyWriter(elemFormat)
      val seqLike = obj.asInstanceOf[Seq[Any]]

      JsArray(seqLike.map(_.toJson(elemWriter)).toVector)

    case formats.MapResultFormat(keyFormat, valueFormat, _, _) if obj.isInstanceOf[Map[_, _]] =>

      val mapLike = obj.asInstanceOf[Map[Any, Any]]
      val valueWriter = new CustomAnyWriter(valueFormat)

      JsObject(mapLike.map { case (key, value) =>
        (serializeKey(key, keyFormat), value.toJson(valueWriter))
      })

    case gf@formats.GraphResultFormat(edgeF, nodeF, _, _) if obj.isInstanceOf[GraphResult] =>
      val graph = obj.asInstanceOf[GraphResult]

      val nodeWriter = new CustomObjectWriter(ListResultFormat(gf.nodeObjectFormat))
      val edgeWriter = new CustomObjectWriter(ListResultFormat(gf.edgeObjectFormat))

      val nodesSerialized = nodeWriter.write(graph.nodes)
      val edgesSerialized = edgeWriter.write(graph.edges)

      JsObject(Map("nodes" -> nodesSerialized, "edges" -> edgesSerialized))

    case formats.ObjectResultFormat(props) =>
      val objectFieldValues = getObjectFieldMap(obj, props.map(_.propertyName)).filter( t => props.exists( p => p.propertyName.equalsIgnoreCase(t._1)))

      JsObject(objectFieldValues.map { case (name, value) =>
        val customWriter = new CustomAnyWriter(props.find(p => p.propertyName.equalsIgnoreCase(name)).get.propertyFormat)

        (name, value.toJson(customWriter))
      })

    case _ => throw DeserializationException("Failed to serialize " + obj + " into " + format)

  }

  private def serializeKey(key: Any, keyFormat: BaseValueFormat): String = keyFormat match {
    case formats.EntityReferenceFormat if key.isInstanceOf[PathIdentifiableJavaEntity] =>
      val entityLike = key.asInstanceOf[PathIdentifiableJavaEntity]
      entityLike.uid
    case _ => key.toString
  }

  /**
   * This method takes an object parameter and produces a map of all field names to their corresponding value. This is done via
   * reflection. This is the only way to define a JsonWrite for spray that serializes arbitrary object values.
   * @param obj Object to extract fields from
   * @return Map of field names to values
   */
  private def getObjectFieldMap(obj: Object, expectedFields: Set[String]) :Map[String, Any] = {

    val typeMirror = runtimeMirror(obj.getClass.getClassLoader)
    val typeInstanceMirror = typeMirror.reflect(obj)
    val symbol = typeInstanceMirror.symbol
    val isCaseClass = symbol.isCaseClass

    val rm = scala.reflect.runtime.currentMirror

    val cs = rm.classSymbol(obj.getClass)
    val tm = cs.toType.members


    val accessors = tm.filter( s => expectedFields.contains(s.name.toString.trim) && s.isTerm  && (s.asTerm.isGetter || s.asTerm.isVal)).map(_.asTerm)

    val instanceMirror = rm.reflect(obj)

    val result = accessors.map(instanceMirror.reflectField)

    result.map(r => (r.symbol.name.toString.trim, r.get)).toMap
  }

  /**
   * Utility JSON writer to serialize values of type 'Any'. Invokes the CustomObjectWriter for any object, and deals with
   * serializing non-object values directly.
   * @param format Format definition for the current value
   */
  private class CustomAnyWriter(format: AnyValueFormat) extends JsonWriter[Any] {

    override def write(obj: Any): JsValue = obj match {
      case obj: Object =>
        new CustomObjectWriter(format).write(obj)
      case i: Int => JsNumber(i)
      case l: Long => JsNumber(l)
      case f: Float => JsNumber(f)
      case d: Double => JsNumber(d)
      case b: Boolean => JsBoolean(b)
    }


  }
}
