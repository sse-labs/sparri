package de.tudo.sse.spareuse.core

package object formats {

  case class NamedPropertyFormat(propertyName: String, propertyFormat: AnyValueFormat)


  //TODO: See how this would be used first


  sealed trait AnyValue[S]{
    def asScalaObject: S
  }

  sealed abstract class AnalysisResult[S] extends AnyValue[S]

  final class ListResult[E <: AnyValue[_]] extends AnalysisResult[List[E]]{
    override def asScalaObject: List[E] = ???
  }

  final class MapResult[K <: BaseValue[_], V <: AnyValue[_]] extends AnalysisResult[Map[K, V]]{
    override def asScalaObject: Map[K, V] = ???
  }

  final class GraphResult



  sealed abstract class BaseValue[T] extends AnyValue[T]

  class StringValue extends BaseValue[String]{
    override def asScalaObject: String = ???
  }

  class NumberValue extends BaseValue[Long]{
    override def asScalaObject: Long = ???
  }

}
