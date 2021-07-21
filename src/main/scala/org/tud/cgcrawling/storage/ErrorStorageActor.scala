package org.tud.cgcrawling.storage

import akka.actor.{Actor, Props}
import org.neo4j.driver.Session
import org.neo4j.driver.Values.parameters
import org.tud.cgcrawling.{AppLogging, Configuration}
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.download.MavenDownloadActorResponse
import org.tud.cgcrawling.graphgeneration.CallGraphActorResponse

class ErrorStorageActor(configuration: Configuration) extends Actor with AppLogging{
  override def receive: Receive = {
    case x: MavenDownloadActorResponse =>
      // Make sure errors while storing failures do never affect program execution!
      try{
        if(x.jarDownloadFailed || x.artifact.isEmpty || x.artifact.get.jarFile.isEmpty){
          storeFailedDownloadForIdentifier(x.identifier)
        }
      } finally {
        sender() ! x
      }

    case x: CallGraphActorResponse =>
      try{
        if(!x.success || x.callgraph.isEmpty || x.callgraph.get.reachableMethods().isEmpty || x.project.isEmpty){
          storeCGErrorForIdentifier(x.artifact.identifier)
        }
      } finally {
        sender() ! x
      }


  }

  private def storeFailedDownloadForIdentifier(identifier: MavenIdentifier): Unit = {
    val session: Session = configuration.graphDatabaseDriver.session()

    session.run("MERGE (f :Failure :DownloadFailure {gav: $gav})",
      parameters("gav", identifier.toString))

    session.close()
  }

  private def storeCGErrorForIdentifier(identifier: MavenIdentifier): Unit = {
    val session: Session = configuration.graphDatabaseDriver.session()

    session.run("MERGE (f :Failure :CallGraphFailure {gav: $gav})",
      parameters("gav", identifier.toString))

    session.close()
  }
}

object ErrorStorageActor {
  def props(config: Configuration): Props = Props(new ErrorStorageActor(config))
}
