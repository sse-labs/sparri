package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.formats.AnalysisResult
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaProgram
import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisResultData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.opal.OPALProjectHelper
import de.tudo.sse.spareuse.core.utils.http.HttpDownloadException
import de.tudo.sse.spareuse.execution.analyses.AnalysisImplementation
import de.tudo.sse.spareuse.execution.analyses.impl.MvnPartialCallgraphAnalysisImpl.{parseConfig, validCallgraphAlgorithms}
import org.opalj.tac.cg.{CHACallGraphKey, CTACallGraphKey, RTACallGraphKey, XTACallGraphKey}

import scala.util.{Failure, Success, Try}

class MvnPartialCallgraphAnalysisImpl extends AnalysisImplementation {

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis("mvn-partial-callgraphs", "1.0.0", "TBD", "OPAL", Set("java", "scala"),
    null, SoftwareEntityKind.Program)


  override val inputBatchProcessing: Boolean = true

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {
    val parts = rawConfig.split(" ")

    for(i <- Range(0, parts.length)){
      if(parts(i).toLowerCase.equals("--algorithm")){
        if( i >= parts.length - 1 || !validCallgraphAlgorithms.exists( algo => algo.toLowerCase.equals(parts(i + 1)))) return false
      } else if(!parts(i).toLowerCase.equals("--use-jre") && !parts(i).equals("--application-mode")) return false
    }

    inputs.forall( sed => sed.isInstanceOf[JavaProgram])
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResultData]] = Try {

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
          val classes = opalHelper.readClassesFromJarStream(inputStream, ???, loadImplementation = true).get
          val project = opalHelper.buildOPALProject(classes, List.empty, config.includeJre, setLibraryMode = !config.applicationMode)

          val cg = project.get(opalCgKey)

          //TODO: Convert to result structure (graph)
          val resultData = AnalysisResultData(isRevoked = false, AnalysisResult.fromObject(List.empty[String]), Set(program))

          Some(resultData)
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

    val parts = raw.split(" ")

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



}
