package org.anon.spareuse.client.analyses

import org.anon.spareuse.client.http.SparriApiClient
import org.anon.spareuse.core.maven.MavenDependencyIdentifier
import org.anon.spareuse.core.utils.EnhancedLogging

import java.io.File

abstract class ClientAnalysis(classFilesDirectory: File, pomFile: File) extends EnhancedLogging with AutoCloseable {

  val api: SparriApiClient = new SparriApiClient

  override def close(): Unit = api.close()



  protected[analyses] def getAllDependencies: Set[MavenDependencyIdentifier] = ???

  protected[analyses] def getAllProjectClassFiles: Set[File] = ???


  protected[analyses] def requirements: Seq[AnalysisRequirement]

  protected[analyses] final def requirementsFullfilled(): Boolean = {
    requirements.forall{ requirement =>
        log.debug(s"Making sure ${requirement.analysisName}:${requirement.analysisVersion} ran for input ${requirement.input} ...")
        api.analysisExecutedWith(requirement.analysisName, requirement.analysisVersion, requirement.input)
      }
  }

  protected[analyses] case class AnalysisRequirement(input: String, analysisName: String, analysisVersion: String)

}
