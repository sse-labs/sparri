package org.tud.cgcrawling.graphgeneration

import akka.actor.ActorSystem
import org.opalj.br.analyses.Project
import org.opalj.tac.cg.CallGraph
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.download.MavenJarDownloadResult
import org.tud.cgcrawling.{AppLogging, Configuration}

import java.net.URL
import java.util.jar.JarInputStream
import scala.util.{Failure, Success, Try}

class CallGraphBuilder(config: Configuration)(implicit system: ActorSystem) extends ClassStreamReader with AppLogging {

  def buildCallgraph(jarFile: MavenJarDownloadResult): CallGraphBuilderResult = {
    Try(reifyProject(jarFile, true)) match {
      case Success(project) =>
        log.info(s"Successfully initialized OPAL project for ${jarFile.identifier.toString}")

        Try(project.get(config.CallGraphAlgorithm)) match {
          case Success(callgraph) =>
            log.info(s"Successfully generated Callgraph for ${jarFile.identifier.toString}")
            CallGraphBuilderResult(jarFile.identifier, success = true, Some(callgraph), Some(project))
          case Failure(ex) =>
            log.error(ex,s"Failed to generate Callgraph for ${jarFile.identifier.toString}")
            CallGraphBuilderResult(jarFile.identifier, success = false, None, None)
        }
      case Failure(ex) =>
        log.error(ex, s"Error while analyzing JAR for artifact ${jarFile.identifier.toString}")
        CallGraphBuilderResult(jarFile.identifier, success = false, None, None)
    }
  }

  private def reifyProject(m: MavenJarDownloadResult, loadAsLibraryProject: Boolean): Project[URL] = {
    val project = createProject(m.identifier.toJarLocation.toURL,
      new JarInputStream(m.jarFile.get.is), loadAsLibraryProject)
    Try(m.jarFile.get.is.close())
    project
  }

}

case class CallGraphBuilderResult(identifier: MavenIdentifier,
                                  success: Boolean,
                                  callgraph: Option[CallGraph],
                                  project: Option[Project[URL]])