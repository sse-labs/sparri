package de.tudo.sse.spareuse.execution.analyses

import de.tudo.sse.spareuse.core.maven.{MavenIdentifier, MavenJarDownloader, MavenReleaseListDiscovery}
import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisResultData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities._
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import org.slf4j.{Logger, LoggerFactory}

import java.io.InputStream
import scala.annotation.tailrec
import scala.util.{Failure, Try}

trait AnalysisImplementation extends MavenReleaseListDiscovery {

  protected val log: Logger = LoggerFactory.getLogger(getClass)

  val analysisData: AnalysisData

  lazy val name: String = analysisData.name
  lazy val version: String = analysisData.version

  lazy val inputEntityKind: SoftwareEntityKind = analysisData.inputKind


  /**
   * Specifies whether this analysis processes the set of inputs one after another (batch processing) or the set of
   * inputs as-a-whole. If set to true, the runner can optimize and remove redundant inputs before execution.
   */
  val inputBatchProcessing: Boolean

  /**
   * Specifies how deep the input entity structure needs to be, i.e. how much information the analysis needs
   * from the database. By default, entities will be resolved until the Method level (thus excluding instructions).
   */
  val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Method

  def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean

  def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResultData]]

  protected def getFilesFor(jl: JavaLibrary): Try[Map[String, InputStream]] = Try {
    val ga = jl.name
    val downloader = new MavenJarDownloader

    val map = getVersionsForLibrary(ga).get.map { version =>
      val gav = ga + ":" + version
      downloader.downloadJar(MavenIdentifier.fromGAV(gav).get).map(result => (version, result.content)).get
    }.toMap

    downloader.shutdown()

    map
  }

  protected def getFileFor(input: SoftwareEntityData): Try[InputStream] =  {


    @tailrec
    def getParentProgram(sed: SoftwareEntityData): Option[JavaProgram] = {
      sed match {
        case jp: JavaProgram => Some(jp)
        case _ if sed.hasParent =>
          getParentProgram(sed.getParent.get)
        case _ => None
      }
    }

    val programIdent: Option[MavenIdentifier] = getParentProgram(input).flatMap(jp => MavenIdentifier.fromGAV(jp.name))

    if(programIdent.isEmpty){
      Failure(new Exception(s"No program GAV could be identified for entity $input"))
    } else {
      val downloader: MavenJarDownloader = new MavenJarDownloader

      val res = downloader.downloadJar(programIdent.get)

      downloader.shutdown()

      res.map(_.content)
    }

  }

}
