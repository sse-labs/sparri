package org.anon.spareuse.client.analyses

import org.anon.spareuse.client.http.SparriApiClient
import org.anon.spareuse.core.maven.{MavenDependencyIdentifier, MavenIdentifier}
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.utils.EnhancedLogging
import org.apache.maven.shared.invoker.{DefaultInvocationRequest, DefaultInvoker}
import org.opalj.br.ClassFile
import org.opalj.br.analyses.Project
import org.opalj.bytecode.JRELibraryFolder

import java.io.File
import java.net.URL
import java.nio.file.Files
import java.util
import java.util.Properties
import java.util.regex.Pattern
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

abstract class ClientAnalysis[T](classFilesDirectory: File, pomFile: File) extends EnhancedLogging with AutoCloseable {

  protected[analyses] val api: SparriApiClient = new SparriApiClient

  override def close(): Unit = api.close()


  def initialize(): Unit = {

    if(!pomFile.exists() || !pomFile.isFile)
      throw new IllegalStateException(s"Invalid pom.xml file specified: ${pomFile.getAbsolutePath}")

    if(!classFilesDirectory.exists() || !classFilesDirectory.isDirectory)
      throw new IllegalStateException(s"Invalid class file directory specified: ${classFilesDirectory.getAbsolutePath}")

    log.info(s"Successfully initialized.")
  }

  def execute(): Try[T]

  protected[analyses] def getAllDependencies: Try[Set[MavenDependencyIdentifier]] = {
    val outFile = Files.createTempFile("dependencies", ".txt")
    Try{
      val request = new DefaultInvocationRequest
      request.setPomFile(pomFile)
      request.setGoals(util.Arrays.asList("dependency:list"))
      val props = new Properties
      props.setProperty("outputFile", outFile.toAbsolutePath.toString)
      props.setProperty("outputAbsoluteArtifactFilename", "true")
      props.setProperty("includeScope", "runtime")
      request.setProperties(props)

      val invoker = new DefaultInvoker
      invoker.setMavenHome(new File("C:\\Program Files\\Java\\apache-maven-3.8.1")) //TODO: Make this configurable
      invoker.setOutputHandler(null)
      val result = invoker.execute(request)

      if(result.getExitCode != 0)
        throw new IllegalStateException(s"Failed to invoke maven")

      val resultLines = Files.readAllLines(outFile)

      val pattern = Pattern.compile("(.*?):(.*?):(.*?)(:compile|runtime):(.*)")
      val startAt = resultLines.asScala.zipWithIndex.find(t => t._1.equalsIgnoreCase("The following files have been resolved:")).map(_._2 + 1).getOrElse(0)

      resultLines.asScala.splitAt(startAt)._2.map { line =>
        val matcher = pattern.matcher(line)

        if(matcher.find()){
          val groupId = matcher.group(1)
          val artifactId = matcher.group(2)
          val rest = matcher.group(3)
          val version = rest.substring(rest.lastIndexOf(":") + 1)
          val scope = matcher.group(4).substring(1)
          val gav = s"$groupId:$artifactId:$version".trim
          log.info(s"Found project dependency: $gav")
          Some(MavenDependencyIdentifier(MavenIdentifier.fromGAV(gav).get, scope))
        } else None
      }
        .filter(_.isDefined)
        .map(_.get)
        .toSet
    }
  }

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
