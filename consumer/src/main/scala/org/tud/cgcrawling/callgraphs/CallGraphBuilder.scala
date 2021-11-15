package org.tud.cgcrawling.callgraphs

import akka.actor.ActorSystem
import org.opalj.br.analyses.Project
import org.opalj.tac.cg.{CallGraph, RTACallGraphKey}
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.download.MavenDownloadResult
import org.tud.cgcrawling.opal.OPALProjectHelper
import org.tud.cgcrawling.opal.OPALProjectHelper.ClassList

import java.net.URL
import scala.util.{Failure, Success, Try}

class CallGraphBuilder(val config: Configuration, val system: ActorSystem) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def buildCallgraph(jarFile: MavenDownloadResult, thirdPartyClasses: ClassList): CallGraphBuilderResult = {
    Try(reifyProject(jarFile, thirdPartyClasses)) match {
      case Success(project) =>
        log.info(s"Successfully initialized OPAL project for ${jarFile.identifier.toString}")
        val codeSize = project.allProjectClassFiles.map(_.methodBodies.sum(_.codeSize)).sum
        val callGraphAlgorithmKey = if(codeSize > config.codeSizeCgCutoffBytes){
          log.warn(s"Falling back to RTA because of JAR code size <$codeSize> exceeding limit of <${config.codeSizeCgCutoffBytes}>")
          RTACallGraphKey
        } else {
          config.CallGraphAlgorithm
        }

        Try(project.get(callGraphAlgorithmKey)) match {
          case Success(callgraph) =>
            log.info(s"Successfully generated Callgraph for ${jarFile.identifier.toString}")
            CallGraphBuilderResult(jarFile.identifier, success = true, Some(callgraph), Some(project))
          case Failure(ex) =>
            log.error(s"Failed to generate Callgraph for ${jarFile.identifier.toString}", ex)
            CallGraphBuilderResult(jarFile.identifier, success = false, None, None)
        }
      case Failure(ex) =>
        log.error(s"Error while analyzing JAR for artifact ${jarFile.identifier.toString}", ex)
        CallGraphBuilderResult(jarFile.identifier, success = false, None, None)
    }
  }

  private def reifyProject(m: MavenDownloadResult, thirdPartyClasses: ClassList): Project[URL] = {

    val projectClasses = OPALProjectHelper.readClassesFromJarStream(m.jarFile.get.is, m.identifier.toJarLocation.toURL).get
    val project = OPALProjectHelper.buildOPALProject(projectClasses, thirdPartyClasses)

    Try(m.jarFile.get.is.close()) match {
      case Failure(ex) => log.error("Failed to close input stream", ex)
      case _ =>
    }

    project
  }

}

case class CallGraphBuilderResult(identifier: MavenIdentifier,
                                  success: Boolean,
                                  callgraph: Option[CallGraph],
                                  project: Option[Project[URL]])