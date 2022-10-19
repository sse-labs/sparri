package de.tudo.sse.spareuse.core.formats

abstract class AnalysisResultFormat(explanation: String) extends AnyValueFormat

case class ListResultFormat(elementFormat: AnyValueFormat,
                       explanation: String = "") extends AnalysisResultFormat(explanation)

case class MapResultFormat(keyFormat: AnyValueFormat,
                      valueFormat: AnyValueFormat,
                      explanation: String = "") extends AnalysisResultFormat(explanation)

case class GraphResultFormat(edgePropertyFormat: Seq[NamedPropertyFormat],
                        nodePropertyFormat: Seq[NamedPropertyFormat],
                        explanation: String = "") extends AnalysisResultFormat(explanation)

case class ObjectResultFormat(propertyFormats:Set[NamedPropertyFormat],
                              explanation: String = "") extends AnalysisResultFormat(explanation)
