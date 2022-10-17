package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.maven.dependencies.{DependencyExtractor, JekaDependencyExtractor, PomFileDependencyExtractor}
import de.tudo.sse.spareuse.core.model.{AnalysisRunData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaProgram
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.execution.analyses.AnalysisImplementation

import scala.util.{Failure, Success, Try}

class MvnDependencyAnalysis(configString: String) extends AnalysisImplementation(configString){

  private var stopRequested = false

  //TODO: Generate a run id!
  private val runId = "12345"

  override val name: String = "mvn-dependencies"
  override val version: String = "1.0.0"
  override val inputEntityKind: SoftwareEntityKind = SoftwareEntityKind.Program

  override def executionPossible(inputs: Seq[SoftwareEntityData]): Boolean = {
    if(inputs.exists( e => !e.isInstanceOf[JavaProgram])){
      log.warn(s"Execution of analysis $name:$version not possible: Inputs must be of kind 'Program'")
      false
    } else if(!isValidConfig){
      log.warn(s"Execution of analysis $name:$version not possible: Configuration not valid: $configString")
      false
    } else {
      true
    }
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData]): Try[AnalysisRunData] = Try {
    val theConfig = parseConfig()
    val resolver: DependencyExtractor = if(theConfig.useJeka) new JekaDependencyExtractor() else new PomFileDependencyExtractor()

    val inputItr = inputs.iterator

    //TODO: Implement GAV + Results

    log.info(s"Analysis $name:$version started. Processing ${inputs.size} input entities...")

    while(inputItr.hasNext && !stopRequested){
      val currentInput = inputItr.next().asInstanceOf[JavaProgram]

      val results = if (theConfig.disableTransitive) resolver.resolveDependencies(???) else resolver.resolveAllDependencies(???)._1

      results match {
        case Success(dependencies) =>
        case Failure(ex) => throw ex
      }
    }

    ???

  }

  override def stopExecution(): Unit = {
    log.info(s"Stop requested for run [$runId] of analysis $name:$version.")
    stopRequested = true
  }

  private def isValidConfig: Boolean = {
    val opts = configString.split(" ")

    opts.map(_.toLowerCase).map {
      case "-use-jeka" => true
      case "-no-transitive" => true
      case _ => false
    }.exists(valid => !valid)
  }

  private def parseConfig(): DependencyAnalysisConfig = {
    val opts = configString.split(" ").map(_.toLowerCase)

    val useJeka = opts.exists(_.equals("-use-jeka"))
    val disableTransitive = opts.exists(_.equals("-no-transitive"))

    DependencyAnalysisConfig(useJeka, disableTransitive)
  }

  private case class DependencyAnalysisConfig(useJeka: Boolean, disableTransitive: Boolean)


}
