package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.maven.{MavenIdentifier, MavenJarDownloader, MavenReleaseListDiscovery}
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities._
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.slf4j.{Logger, LoggerFactory}

import java.io.InputStream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Try}

trait AnalysisImplementation extends MavenReleaseListDiscovery {

  private val internalLog: Logger = LoggerFactory.getLogger(getClass)
  protected val log: PersistingLogger = new PersistingLogger

  val descriptor: AnalysisImplementationDescriptor

  def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean

  def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[Result]]

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

  def getLogs: Seq[String] = log.getLogs

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

  class PersistingLogger {

    private val logs: mutable.ListBuffer[String] = mutable.ListBuffer.empty

    private final val DEBUG_PREFIX = "[DEBUG] "
    private final val INFO_PREFIX = "[INFO] "
    private final val WARN_PREFIX = "[WARN] "
    private final val ERROR_PREFIX = "[ERROR] "

    def debug(msg: String): Unit = {
      internalLog.debug(msg)
      if(internalLog.isDebugEnabled) logs.append(DEBUG_PREFIX + msg)
    }

    def info(msg: String): Unit = {
      internalLog.info(msg)
      logs.append(INFO_PREFIX + msg)
    }

    def warn(msg: String, ex: Throwable = null): Unit = {
      if(ex == null){
        internalLog.warn(msg)
        logs.append(WARN_PREFIX + msg)
      } else {
        internalLog.warn(msg, ex)
        logs.append(WARN_PREFIX + msg + s" ( ${ex.getClass} : ${ex.getMessage})")
      }

    }

    def error(msg: String, ex: Throwable = null): Unit ={
      if(ex == null){
        internalLog.error(msg)
        logs.append(ERROR_PREFIX + msg)
      } else {
        internalLog.error(msg, ex)
        logs.append(ERROR_PREFIX + msg + s" ( ${ex.getClass} : ${ex.getMessage})")
      }
    }

    def getLogs: Seq[String] = logs

  }
}

trait AnalysisImplementationDescriptor {

  val analysisData: AnalysisData

  lazy val name: String = analysisData.name
  lazy val version: String = analysisData.version
  lazy val fullName: String = s"$name:$version"

  lazy val inputEntityKind: SoftwareEntityKind = analysisData.inputKind

  lazy val isIncremental: Boolean = analysisData.isIncremental


  /**
   * Specifies whether this analysis processes the set of inputs one after another (batch processing) or the set of
   * inputs as-a-whole. If set to true, the runner can optimize and remove redundant inputs before execution.
   */
  final lazy val inputBatchProcessing: Boolean = analysisData.doesBatchProcessing

  /**
   * Specifies how deep the input entity structure needs to be, i.e. how much information the analysis needs
   * from the database. By default, entities will be resolved until the Method level (thus excluding instructions).
   */
  val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Method
}
