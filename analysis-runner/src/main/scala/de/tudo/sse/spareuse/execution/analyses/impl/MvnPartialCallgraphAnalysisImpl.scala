package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.formats
import de.tudo.sse.spareuse.core.formats.{AnalysisResultFormat, EmptyFormat, GraphResultFormat, NamedPropertyFormat}
import de.tudo.sse.spareuse.core.maven.MavenIdentifier
import de.tudo.sse.spareuse.core.model.analysis.{EdgeWithIds, GraphResult, NodeWithId}
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaProgram
import de.tudo.sse.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.opal.OPALProjectHelper
import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import de.tudo.sse.spareuse.execution.analyses.{AnalysisImplementation, Result}
import de.tudo.sse.spareuse.execution.analyses.impl.MvnPartialCallgraphAnalysisImpl.{InternalEdge, InternalGraph, InternalNode, parseConfig, validCallgraphAlgorithms}
import org.opalj.tac.cg.{CHACallGraphKey, CTACallGraphKey, RTACallGraphKey, XTACallGraphKey}

import scala.util.{Failure, Success, Try}

class MvnPartialCallgraphAnalysisImpl extends AnalysisImplementation {

  private val resultFormat: AnalysisResultFormat = GraphResultFormat(Set(NamedPropertyFormat("callSitePc", formats.NumberFormat)),Set(NamedPropertyFormat("methodName", formats.StringFormat)), "", "")

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis("mvn-partial-callgraphs", "1.0.0", "TBD", "OPAL", Set("java", "scala"),
    resultFormat, SoftwareEntityKind.Program)


  override val inputBatchProcessing: Boolean = true

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {

    if(rawConfig.trim.isBlank) return true

    val parts = rawConfig.trim.split(" ")

    for(i <- Range(0, parts.length)){
      if(parts(i).toLowerCase.equals("--algorithm")){
        if( i >= parts.length - 1 || !validCallgraphAlgorithms.exists( algo => algo.toLowerCase.equals(parts(i + 1)))) return false
      } else if(!parts(i).toLowerCase.equals("--use-jre") && !parts(i).equals("--application-mode")) return false
    }

    inputs.forall( sed => sed.isInstanceOf[JavaProgram])
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[Result]] = Try {

    val opalHelper = new OPALProjectHelper(loadJreClassImplementation = false)
    val config = parseConfig(rawConfig)

    val opalCgKey = config.algorithm match {
      case "cha" => CHACallGraphKey
      case "rta" => RTACallGraphKey
      case "xta" => XTACallGraphKey
      case "cta" => CTACallGraphKey
      case a@_ => {
        log.warn(s"Invalid CG key after validation: $a")
        XTACallGraphKey
      }
    }


    inputs.map( sed => sed.asInstanceOf[JavaProgram] ).flatMap { program =>
      getFileFor(program) match {
        case Success(inputStream) =>
          val jarUrl = MavenIdentifier.fromGAV(program.name).map(_.toJarLocation.toURL).get
          val classes = opalHelper.readClassesFromJarStream(inputStream, jarUrl, loadImplementation = true).get
          val project = opalHelper.buildOPALProject(classes, List.empty, config.includeJre, setLibraryMode = !config.applicationMode)

          val cg = project.get(opalCgKey)

          var nodeId = 0

          val nodeLookup = cg.reachableMethods().map(m => {
            val tuple = (m, nodeId)
            nodeId += 1
            tuple
          }).toMap

          val edgesIt = cg.reachableMethods().flatMap { rm =>
            val sourceId = nodeLookup(rm)

            cg.calleesOf(rm).flatMap{ callee =>
              val pc = callee._1
              callee._2.flatMap { target =>
                if(nodeLookup.contains(target)){
                  val targetId = nodeLookup(target)
                  Some(new InternalEdge(sourceId, targetId, pc))
                } else None
              }
            }
          }

          val explicitGraph: GraphResult = new InternalGraph(nodeLookup.map(t => new InternalNode(t._2, t._1.toJava)).toSeq, edgesIt.toSeq)

          Some(Result(explicitGraph, Set(program)))
        case Failure(HttpDownloadException(status, _, _)) if status == 404 =>
          log.warn(s"No JAR file available for ${program.identifier}")
          None
        case Failure(ex) =>
          log.error(s"Failed to download JAR file contents for ${program.identifier}", ex)
          throw ex

      }
    }.toSet

  }
}

object MvnPartialCallgraphAnalysisImpl {

  def validCallgraphAlgorithms: Set[String] = Set("cha", "rta", "cta", "xta")

  case class PartialCallgraphAnalysisConfig(algorithm: String, includeJre: Boolean, applicationMode: Boolean)

  def parseConfig(raw: String): PartialCallgraphAnalysisConfig = {
    var algo = "xta"
    var includeJre = false
    var appMode = false

    val parts = raw.trim.split(" ")

    for (i <- Range(0, parts.length)) {
      if (parts(i).toLowerCase.equals("--algorithm")) {
        algo = parts(i + 1)
      } else if (parts(i).toLowerCase.equals("--use-jre") ) {
        includeJre = true
      } else if(parts(i).toLowerCase.equals("--application-mode")){
        appMode = true
      }
    }

    PartialCallgraphAnalysisConfig(algo, includeJre, appMode)
  }

  class InternalGraph(nodeSeq: Seq[InternalNode], edgeSeq: Seq[InternalEdge]) extends GraphResult{
    override val nodes: Seq[NodeWithId] = nodeSeq
    override val edges: Seq[EdgeWithIds] = edgeSeq
  }

  class InternalEdge(source: Int, target: Int, pc: Int) extends EdgeWithIds {
    override val __from__ : Long = source
    override val __to__ : Long = target

    val callSitePc: Int = pc
  }

  class InternalNode(id: Int, name: String) extends NodeWithId {
    override val __uid__ : Long = id

    val methodName: String = name
  }

}
