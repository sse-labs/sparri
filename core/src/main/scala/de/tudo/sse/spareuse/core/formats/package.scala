package de.tudo.sse.spareuse.core


import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData

package object formats {



  //TODO: See how this would be used first


  sealed trait AnyValue[S]{
    def asScalaObject: S

    def valueFormat: AnyValueFormat
  }

  sealed abstract class AnalysisResult[S] extends AnyValue[S]

  final class ListResult[E <: AnyValue[_]](value: List[E]) extends AnalysisResult[List[E]]{
    override def asScalaObject: List[E] = value

    override def valueFormat: AnyValueFormat = {
      val innerFormat = if(value.nonEmpty) value.head.valueFormat else EmptyFormat

      ListResultFormat(innerFormat)
    }
  }

  final class MapResult[K <: BaseValue[_], V <: AnyValue[_]](value: Map[K, V]) extends AnalysisResult[Map[K, V]]{
    override def asScalaObject: Map[K, V] = value

    override def valueFormat: AnyValueFormat = {
      if(value.nonEmpty){
        MapResultFormat(value.head._1.valueFormat, value.head._2.valueFormat)
      } else {
        MapResultFormat(EmptyFormat, EmptyFormat)
      }
    }
  }

  final class GraphResult //TODO: Graph
  //TODO: Object

  sealed class EntityReference(uidRef: String, kind: SoftwareEntityKind) extends AnyValue[(String, SoftwareEntityKind)] {
    override def asScalaObject: (String, SoftwareEntityKind) = (uidRef, kind)

    override def valueFormat: AnyValueFormat = EntityReferenceFormat
  }



  sealed abstract class BaseValue[T] extends AnyValue[T]

  class StringValue(value: String) extends BaseValue[String]{
    override def asScalaObject: String = value

    override def valueFormat: AnyValueFormat = StringFormat
  }

  class NumberValue(value: Long) extends BaseValue[Long]{
    override def asScalaObject: Long = value

    override def valueFormat: AnyValueFormat = NumberFormat
  }

  object AnalysisResult {
    def fromList [T <: AnyValue[_]] (obj: List[T]): ListResult[T] =
      new ListResult[T](obj)

    def fromMap [K <: BaseValue[_], V <: AnyValue[_]] (obj: Map[K, V]): MapResult[K, V] =
      new MapResult[K, V](obj)



    def baseValueFromObject(obj: Any): BaseValue[_] = obj match {
      case s: String => new StringValue(s)
      case i: Int => new NumberValue(i)
      case l: Long => new NumberValue(l)

      case _ =>
        throw new IllegalValueException("Expected String, Int or Long", obj)
    }

    def fromObject(obj: Any): AnyValue[_] = obj match {
      case s: String => new StringValue(s)
      case i: Int => new NumberValue(i)
      case l: Long => new NumberValue(l)

      case list: Seq[_] => fromList(list.map(o => fromObject(o)).toList)
      case map: Map[_,_] => fromMap(map.map(t => (baseValueFromObject(t._1), fromObject(t._2))).toMap[BaseValue[_], AnyValue[_]])

      case e: SoftwareEntityData => new EntityReference(e.uid, e.kind)

      case a: AnyValue[_] => a

      case _ =>
        throw new IllegalValueException("Expected String, Int, Long, Seq[_] or Map[_]", obj)
    }
  }

  class IllegalValueException(msg: String, obj: Any) extends Throwable {
    override def getMessage: String = {
      s"Illegal Value of type ${obj.getClass}: $msg"
    }
  }

}
