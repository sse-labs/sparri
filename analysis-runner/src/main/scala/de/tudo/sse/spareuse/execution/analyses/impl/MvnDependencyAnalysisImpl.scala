package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.formats.AnalysisResult
import de.tudo.sse.spareuse.core.maven.MavenIdentifier
import de.tudo.sse.spareuse.core.maven.dependencies.{DependencyExtractor, JekaDependencyExtractor, PomFileDependencyExtractor}
import de.tudo.sse.spareuse.core.model.{AnalysisResultData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaProgram, buildProgram}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.execution.analyses.AnalysisImplementation

import scala.util.{Failure, Success, Try}

class MvnDependencyAnalysisImpl extends AnalysisImplementation{


  override val name: String = "mvn-dependencies"
  override val version: String = "1.0.0"
  override val inputEntityKind: SoftwareEntityKind = SoftwareEntityKind.Program

  // This analysis does not need any DB information
  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Program

  override def executionPossible(inputs: Seq[SoftwareEntityData], configRaw: String): Boolean = {

    if(inputs.exists( e => !e.isInstanceOf[JavaProgram])){
      log.warn(s"Execution of analysis $name:$version not possible: Inputs must be of kind 'Program'")
      false
    } else if(!isValidConfig(configRaw)){
      log.warn(s"Execution of analysis $name:$version not possible: Configuration not valid: $configRaw")
      false
    } else {
      true
    }
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], configRaw: String): Try[Set[AnalysisResultData]] = Try {
    val theConfig = parseConfig(configRaw)
    val resolver: DependencyExtractor = if(theConfig.useJeka) new JekaDependencyExtractor() else new PomFileDependencyExtractor()

    inputs.map {
      case jp: JavaProgram =>

        val gav = MavenIdentifier.fromGAV(jp.name)
          .getOrElse(throw new IllegalArgumentException("Java Program name is not a GAV triple"))

        val dependenciesTry = if(theConfig.disableTransitive)
          resolver.resolveDependencies(gav)
        else
          resolver.resolveAllDependencies(gav)._1

        dependenciesTry match {
          case Success(dependencies) =>
            log.debug(s"Extracted ${dependencies.size} dependencies for program $gav")

            // TODO: Needs object format
            val resultList = dependencies.map( dep => Map("gav" -> buildProgram(dep.identifier.toString), "scope" -> dep.scope)).toList

            AnalysisResultData(isRevoked = false, AnalysisResult.fromObject(resultList), Set(jp))
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
        case "-no-transitive" => true
        case _ => false
      }.forall(valid => valid)
    }

  }

  private def parseConfig(configRaw: String): DependencyAnalysisConfig = {
    val opts = configRaw.split(" ").map(_.toLowerCase.trim)

    val useJeka = opts.exists(_.equals("-use-jeka"))
    val disableTransitive = opts.exists(_.equals("-no-transitive"))

    DependencyAnalysisConfig(useJeka, disableTransitive)
  }

  private case class DependencyAnalysisConfig(useJeka: Boolean, disableTransitive: Boolean)


}
