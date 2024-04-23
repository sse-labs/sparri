package org.anon.spareuse.client.analyses

import org.anon.spareuse.client.http.SparriApiClient
import org.anon.spareuse.core.maven.MavenDependencyIdentifier
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.utils.EnhancedLogging
import org.opalj.br.ClassFile
import org.opalj.br.analyses.Project
import org.opalj.bytecode.JRELibraryFolder

import java.io.File
import java.net.URL
import scala.util.{Failure, Success, Try}

abstract class ClientAnalysis(classFilesDirectory: File, pomFile: File) extends EnhancedLogging with AutoCloseable {

  protected[analyses] val api: SparriApiClient = new SparriApiClient

  override def close(): Unit = api.close()

  protected[analyses] def getAllDependencies: Set[MavenDependencyIdentifier] = ???

  protected[analyses] def getOpalProject(loadJre: Boolean): Project[URL] = {
    if(!classFilesDirectory.isDirectory || !classFilesDirectory.exists())
      throw new IllegalStateException(s"Classfile directory invalid: ${classFilesDirectory.getAbsolutePath}")

    if(!loadJre) Project(classFilesDirectory)
    else Project(classFilesDirectory, JRELibraryFolder)
  }

  protected[analyses] def getProjectModel(projectClassFiles: Seq[ClassFile]): Try[JavaProgram] = Try {
    val projectName = classFilesDirectory.getName
    OPALJavaConverter.convertProgram(s"custom:$projectName:1.0.0-SNAPSHOT", "<no-repo>", projectClassFiles.toList, "<no-upload>")
  }


  protected[analyses] def requirements: Seq[AnalysisRequirement]

  final def checkRequirements(): Boolean = {
    timedOp("Validating requirements", () => Try {
      requirements.forall { requirement =>
        log.debug(s"Checking requirement for input ${requirement.input} ...")
        val r = api.analysisExecutedWith(requirement.analysisName, requirement.analysisVersion, requirement.input)

        if(!r) log.error(s"Requirement not satisfied: Analysis ${requirement.analysisName}:${requirement.analysisVersion} did not run for input ${requirement.input}")
        else log.debug(s"Requirement satisfied: Analysis ${requirement.analysisName}:${requirement.analysisVersion} ran for input ${requirement.input}")
        r
      }
    }) match {
      case Success(result) =>
        result
      case Failure(ex) =>
        log.error(s"Failed to validate requirements for analysis.", ex)
        false
    }
  }

  protected[analyses] case class AnalysisRequirement(input: String, analysisName: String, analysisVersion: String)

}
