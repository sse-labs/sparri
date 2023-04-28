package de.tudo.sse.spareuse.eval.formats

import de.tudo.sse.spareuse.core.formats.{AnalysisResultFormat, GraphResultFormat, ListResultFormat, MapResultFormat, NamedPropertyFormat, NumberFormat, ObjectResultFormat, StringFormat}

object ExternalFormatDefinitions extends App {

  val jipdaFormat: AnalysisResultFormat = ObjectResultFormat(
    NamedPropertyFormat("DiagramName", StringFormat, "The name of this flow graph instance"),
    NamedPropertyFormat("NodeStyle",
      ObjectResultFormat(
        NamedPropertyFormat("style", StringFormat, "The mode in which to draw nodes (i.e. 'filled')"),
        NamedPropertyFormat("fontname", StringFormat, "The name of the font to use when rendering node labels")
      ),
      "The styling to use when displaying nodes"
    ),
    NamedPropertyFormat("FlowGraph",
      GraphResultFormat(
        nodePropertyFormat = Set(NamedPropertyFormat("label", StringFormat, "The label for this node in the flow graph"),
          NamedPropertyFormat("tooltip", StringFormat, "The tooltip for this node"), NamedPropertyFormat("color", StringFormat, "The color in which to render the tooltip")),
        edgePropertyFormat = Set.empty,
        nodeExplanation = "Nodes represent statements in the flow graph"
      ),
      "The actual contents of the flow graph")
  )

  val auxiliaryPointsTo: AnalysisResultFormat = MapResultFormat(
    keyFormat = StringFormat,
    keyExplanation = "The unique name of a program variable",
    valueFormat = ListResultFormat(ObjectResultFormat(
      NamedPropertyFormat("statement", StringFormat, "The textual representation of an allocation statement"),
      NamedPropertyFormat("pc", NumberFormat, "The program counter of the allocation statement")
    ), "Program allocation sites"),
    valueExplanation = "A set of object allocation sites that the variable may point to"
  )

  println("----------JIPDA flow graphs as used by Nicolay et al. (10.1145/2993600.2993612)----------")
  println(jipdaFormat.formatDescription())
  println("----------Auxiliary Points-To Sets as used by Hardekopf and Lin (10.1109/CGO.2011.5764696)----------")
  println(auxiliaryPointsTo.formatDescription())

}
