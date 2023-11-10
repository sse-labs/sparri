package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.formats
import org.anon.spareuse.core.formats.{ListResultFormat, NamedPropertyFormat, ObjectResultFormat}
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.maven.dependencies.{DependencyExtractor, JekaDependencyExtractor, PomFileDependencyExtractor}
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisImplementationDescriptor, AnalysisResult, FreshResult}

import scala.util.{Failure, Success, Try}

class MvnDependencyAnalysisImpl extends AnalysisImplementation{

  override val descriptor: AnalysisImplementationDescriptor = MvnDependencyAnalysisImpl

  override def executionPossible(inputs: Seq[SoftwareEntityData], configRaw: String): Boolean = {

    if(inputs.exists( e => !e.isInstanceOf[JavaProgram])){
      log.warn(s"Execution of analysis ${descriptor.fullName} not possible: Inputs must be of kind 'Program'")
      false
    } else if(!isValidConfig(configRaw)){
      log.warn(s"Execution of analysis ${descriptor.fullName} not possible: Configuration not valid: $configRaw")
      false
    } else {
      true
    }
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], configRaw: String): Try[Set[AnalysisResult]] = Try {
    val theConfig = parseConfig(configRaw)
    val resolver: DependencyExtractor = if(theConfig.useJeka) new JekaDependencyExtractor() else new PomFileDependencyExtractor()

    inputs.map {
      case jp: JavaProgram =>

        log.debug(s"Starting to process program ${jp.name} ...")

        val gav = MavenIdentifier.fromGAV(jp.name)
          .getOrElse(throw new IllegalArgumentException("Java Program name is not a GAV triple"))

        val dependenciesTry = if(theConfig.enableTransitive)
          resolver.resolveAllDependencies(gav)._1
        else
          resolver.resolveDependencies(gav)


        dependenciesTry match {
          case Success(dependencies) =>
            log.debug(s"Results for program [$gav]:")
            dependencies.foreach{ d => log.debug(s"-- ${d.identifier}:${d.scope}")}

            FreshResult(dependencies, Set(jp))
          case Failure(ex) =>
            log.error("Dependency extraction failed", ex)
            throw ex
        }
    }.toSet

  }

  private def isValidConfig(configRaw: String): Boolean = {
    if(configRaw.isBlank){
      true
    } else {
      val opts = configRaw.split(" ")

      opts.map(_.toLowerCase).map {
        case "-use-jeka" => true
        case "-transitive" => true
        case _ => false
      }.forall(valid => valid)
    }

  }

  private def parseConfig(configRaw: String): DependencyAnalysisConfig = {
    val opts = configRaw.split(" ").map(_.toLowerCase.trim)

    val useJeka = opts.exists(_.equals("-use-jeka"))
    val enableTransitive = opts.exists(_.equals("-transitive"))

    DependencyAnalysisConfig(useJeka, enableTransitive)
  }

  private case class DependencyAnalysisConfig(useJeka: Boolean, enableTransitive: Boolean)
}

object MvnDependencyAnalysisImpl extends AnalysisImplementationDescriptor {

  private val resultFormat = ListResultFormat(
    ObjectResultFormat(Set(
      NamedPropertyFormat("identifier", ObjectResultFormat(Set(
        NamedPropertyFormat("groupId", formats.StringFormat),
        NamedPropertyFormat("artifactId", formats.StringFormat),
        NamedPropertyFormat("version", formats.StringFormat)
      )), "The GAV-Triple identifying a dependency, i.e. a required Maven library."),
      NamedPropertyFormat("scope", formats.StringFormat, "The Maven scope of this dependency")
    )), "Dependencies (GAV-Triple) for this program"
  )

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis(
    "mvn-dependencies",
    "1.0.0",
    "TBD",
    "Scala, built-in facilities",
    Set("java"),
    resultFormat,
    SoftwareEntityKind.Program,
    doesBatchProcessing = true,
    isIncremental = false
  )

  // This analysis does not need any DB information
  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Program

}
