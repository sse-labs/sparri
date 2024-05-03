package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.maven.dependencies.PomFileDependencyExtractor
import org.anon.spareuse.core.maven.{MavenDependencyIdentifier, MavenIdentifier, MavenJarDownloader, MavenReleaseListDiscovery}
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities._
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.execution.analyses.impl.MvnDependencyAnalysisImpl
import org.opalj.br.analyses.Project
import org.slf4j.{Logger, LoggerFactory}
import spray.json.{JsArray, JsObject, JsString, JsValue}

import java.io.InputStream
import java.net.URL
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait AnalysisImplementation extends MavenReleaseListDiscovery {

  private val internalLog: Logger = LoggerFactory.getLogger(getClass)
  protected val log: PersistingLogger = new PersistingLogger

  protected[analyses] lazy val dependencyExtractor = new PomFileDependencyExtractor

  val descriptor: AnalysisImplementationDescriptor

  def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean

  def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]]

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

  protected def buildOpalProjectFor(programIdent: MavenIdentifier,
                                    transitiveDependencyIdents: Set[MavenIdentifier]): Try[Project[URL]] = {
    val projectHelper = new OPALProjectHelper
    val downloader = new MavenJarDownloader

    val result = Try {
      val projectIS = downloader.downloadJar(programIdent).get
      val projectClasses = projectHelper
        .readClassesFromJarStream(projectIS.content, programIdent.toJarLocation.toURL, loadImplementation = true)
        .get

      val libraryClasses = transitiveDependencyIdents
        .map { depIdent =>
          Try {
            val dependencyIS = downloader.downloadJar(depIdent).get
            projectHelper
              .readClassesFromJarStream(dependencyIS.content, depIdent.toJarLocation.toURL, loadImplementation = true)
              .get
          }
        }
        .filter {
          case Success(_) => true
          case Failure(ex) =>
            log.error(s"Failed to download dependency for ${programIdent.toString}", ex)
            false
        }
        .flatMap { s => s.get }
        .toList

      projectHelper.buildOPALProject(projectClasses, libraryClasses, loadJre = false, setLibraryMode = true)
    }

    downloader.shutdown()

    result
  }

  protected def computeTransitiveDependencies(program: JavaProgram): Try[dependencyExtractor.Dependencies] = {
    dependencyExtractor.resolveAllDependencies(MavenIdentifier.fromGAV(program.programName).get) match {
      case (succ@Success(_), failures) =>
        failures.foreach{ ident => log.warn(s"Unable to resolve transitive dependency ${ident.toString} while processing ${program.programName}")}
        succ
      case (fail@Failure(ex), _) =>
        log.error(s"Failed to compute transitive dependencies for ${program.programName}", ex)
        fail
    }
  }

  protected def getTransitiveDependencies(dataAccessor: DataAccessor, program: JavaProgram): Try[dependencyExtractor.Dependencies] = {

    import spray.json.enrichString
    implicit def asString(value: JsValue): String = value.asInstanceOf[JsString].value

    log.info(s"Accessing all dependencies for ${program.programName} ...")
    dataAccessor.getJSONResultsFor(program.identifier,
      analysisFilter = Some((MvnDependencyAnalysisImpl.name, MvnDependencyAnalysisImpl.version)),
      limit = 1,
      skip = 0) match {

      // If lookup in DB succeeds and results are found, we return them
      case Success(results) if results.nonEmpty =>
        val resultContent = results.head.content.asInstanceOf[String].parseJson.asInstanceOf[JsArray]

        log.info(s"Found ${resultContent.elements.size} dependencies precomputed in DB.")

        Success(resultContent.elements.collect{
          case jo: JsObject =>
            val identObj = jo.fields("identifier").asInstanceOf[JsObject]
            val ident = MavenIdentifier(MavenIdentifier.DefaultRepository, identObj.fields("groupId"), identObj.fields("artifactId"), identObj.fields("version"))
            MavenDependencyIdentifier(ident, jo.fields("scope"))
        })


      // If lookup in DB succeeds, but no results are found, we compute dependencies now and return them
      // IMPROVE: We may want to store them in the DB here to avoid redundant re-computations
      case Success(_) =>
        log.info(s"No dependencies computed yet - starting analysis locally ...")
       computeTransitiveDependencies(program) match {
         case succ@Success(dependencies) =>
           log.info(s"Found ${dependencies.size} transitive dependencies.")
           succ
         case fail@Failure(_) =>
           log.error(s"Computing dependencies for ${program.programName} failed.")
           fail
       }

      // If looking up results in DB fails, we will re-compute them on-the-fly, but not attempt to store them in the DB
      case Failure(ex) =>
        log.error(s"Failed to check whether list of dependencies for ${program.programName} already exists in DB.", ex)
        log.info(s"Computing dependencies locally ... ")
        computeTransitiveDependencies(program) match {
          case succ@Success(dependencies) =>
            log.info(s"Found ${dependencies.size} transitive dependencies.")
            succ
          case fail@Failure(_) =>
            log.error(s"Computing dependencies for ${program.programName} failed.")
            fail

        }
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

    def getLogs: Seq[String] = logs.toSeq

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
