package org.tud.cgcrawling.graphgeneration

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import org.opalj.br.analyses.Project
import org.opalj.tac.cg.CallGraph
import org.tud.cgcrawling.{AppLogging, Configuration}
import org.tud.cgcrawling.discovery.maven.MavenArtifact

import java.net.URL
import java.util.jar.JarInputStream
import scala.util.{Failure, Success, Try}

class CallGraphActor(config: Configuration) extends Actor with AppLogging with ClassStreamReader {

  private implicit val system: ActorSystem = context.system

  override def receive: Receive = {
    case artifact: MavenArtifact =>
      Try(artifact.pomFile.is.close())
      Try(reifyProject(artifact, true)) match {
        case Success(project) =>
          log.info(s"Successfully initialized OPAL project for ${artifact.identifier.toString}")

          Try(project.get(config.CallGraphAlgorithm)) match {
            case Success(callgraph) =>
              log.info(s"Successfully generated Callgraph for ${artifact.identifier.toString}")
              sender() ! CallGraphActorResponse(artifact, true, Some(callgraph), Some(project))
            case Failure(ex) =>
              log.error(s"Failed to generate Callgraph for ${artifact.identifier.toString}", ex)
              sender() ! CallGraphActorResponse(artifact, false, None, None)
          }
        case Failure(exception) =>
          log.error(s"Error while analyzing JAR for artifact ${artifact.identifier.toString}", exception)
          exception.printStackTrace()
          sender() ! CallGraphActorResponse(artifact, false, None, None)
      }
  }

  private def reifyProject(m: MavenArtifact, loadAsLibraryProject: Boolean): Project[URL] = {

    val project = createProject(m.identifier.toJarLocation.toURL,
      new JarInputStream(m.jarFile.get.is), loadAsLibraryProject)
    Try(m.jarFile.get.is.close())
    project
  }
}

object CallGraphActor {



  def props(config: Configuration): Props = Props(new CallGraphActor(config))

}

case class CallGraphActorResponse(artifact: MavenArtifact,
                                  success: Boolean,
                                  callgraph: Option[CallGraph],
                                  project: Option[Project[URL]])
