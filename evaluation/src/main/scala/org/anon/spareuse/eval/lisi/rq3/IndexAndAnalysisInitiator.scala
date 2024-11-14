package org.anon.spareuse.eval.lisi.rq3

import org.anon.spareuse.client.analyses.ClientAnalysis
import org.anon.spareuse.core.maven.{MavenDependencyIdentifier, MavenIdentifier}
import org.anon.spareuse.core.model.RunState
import org.anon.spareuse.eval
import org.anon.spareuse.execution.analyses.impl.ifds.IFDSTaintFlowSummaryBuilderImpl
import org.apache.http.impl.client.HttpClients

import java.io.File
import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object IndexAndAnalysisInitiator {

  class DependencyAnalysis(classDir: File, pomFile: File) extends ClientAnalysis[Set[MavenDependencyIdentifier]](classDir, pomFile){
    override def execute(): Try[Set[MavenDependencyIdentifier]] = super.getAllDependencies

    override def requirements: Seq[AnalysisRequirement] = Seq.empty
  }


  def getAllDependencies(mavenProjectRoot: File): Try[Set[MavenDependencyIdentifier]] = {
    val classDir = Paths.get(mavenProjectRoot.getPath, "target", "classes").toFile
    val pomFile = Paths.get(mavenProjectRoot.getPath, "pom.xml").toFile

    if(!classDir.exists() || !classDir.isDirectory)
      throw new IllegalStateException(s"Project is not a valid maven project, no classes directory found at ${classDir.getAbsolutePath}")
    if(!pomFile.exists())
      throw new IllegalStateException(s"Project is not a valid maven project, no valid POM file found at ${pomFile.getAbsolutePath}")

    val analysis = new DependencyAnalysis(classDir, pomFile)

    analysis.execute()
  }

  def main(args: Array[String]): Unit = {
    if(args.length != 1 || args(0).isBlank) {
      println(s"ERROR: Missing required argument 'maven project path'")
      println(s"ERROR: Usage: IndexAndAnalysisInitiator <maven-project-path>")
      System.exit(-1)
    }

    val mavenProjectRoot = new File(args(0))

    println(s"Obtaining dependencies for Maven project: ${mavenProjectRoot.getAbsolutePath} ...")
    getAllDependencies(mavenProjectRoot) match {
      case Success(dependencies) =>
        println(s"Successfully got ${dependencies.size} dependencies.")

        val httpClient = HttpClients.createDefault()

        val dependenciesNotInIndex = dependencies
          .filter(_.scope == "compile")
          .filter(dep => eval.triggerEntityMining(dep.identifier.toString, eval.getApiBaseUrl, httpClient).isEmpty)

        if(dependenciesNotInIndex.nonEmpty){
          println(s"Not all entities are indexed yet, wait for ${dependenciesNotInIndex.size} index requests to complete and retry:")
          dependenciesNotInIndex.foreach(dep => println(s"\t - ${dep.identifier.toString}"))
          return
        }

        println(s"All ${dependencies.size} dependencies are indexed. Checking availability of partial results...")

        val dependenciesMissingResults = dependencies
          .filter{ dep =>
            val eid = toEID(dep.identifier)
            eval.getRunsForEntity(eid, IFDSTaintFlowSummaryBuilderImpl.analysisName, "0.0.2", eval.getApiBaseUrl, httpClient) match {
              case Success(allRuns) =>
                !allRuns.exists(_.State == RunState.Finished.toString)
              case Failure(ex) =>
                println(s"ERROR: Failed to retrieve runs: ${ex.getMessage}")
                false
            }
          }

        if(dependenciesMissingResults.nonEmpty){
          dependenciesMissingResults.foreach{ dep =>
            eval.triggerAnalysisRun(Set(toEID(dep.identifier)), IFDSTaintFlowSummaryBuilderImpl.analysisName, "0.0.2", eval.getApiBaseUrl, httpClient) match {
              case Success(runId) =>
                println(s"Successfully triggered analysis run $runId for entity ${dep.identifier.toString}")
              case Failure(ex) =>
                println(s"Error: Failed to trigger analysis run for ${dep.identifier}: ${ex.getMessage}")
            }
          }

          println(s"Partial results are missing for ${dependenciesMissingResults.size} dependencies. Analyses have been queued, wait for completion and retry.")
          return
        }

        println(s"All dependencies are indexed, all partial results are available.")

      case Failure(ex) =>
        println(s"ERROR: Failed to compute dependencies: ${ex.getMessage}")
    }

  }

  private def toEID(ident: MavenIdentifier): String = s"${ident.toGA}!${ident.version}"



}
