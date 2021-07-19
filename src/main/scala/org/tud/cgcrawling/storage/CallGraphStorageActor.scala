package org.tud.cgcrawling.storage

import akka.actor.{Actor, ActorSystem, Props}
import org.neo4j.driver.Session
import org.neo4j.driver.Values.parameters
import org.opalj.br.{DeclaredMethod, VirtualDeclaredMethod}
import org.opalj.br.analyses.Project
import org.opalj.tac.cg.CallGraph
import org.tud.cgcrawling.{AppLogging, Configuration}
import org.tud.cgcrawling.discovery.maven.MavenArtifact
import org.tud.cgcrawling.graphgeneration.CallGraphActorResponse

import java.net.URL
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable

class CallGraphStorageActor(configuration: Configuration) extends Actor
  with AppLogging{

  private implicit val system: ActorSystem = context.system

  override def receive: Receive = {

    case CallGraphActorResponse(artifact, true, Some(cg), Some(project)) =>
      val start = System.currentTimeMillis()
      log.info(s"Storing CallGraph for artifact ${artifact.identifier.toString}")
      val success = storeCallGraph(artifact, cg, project)
      val duration = System.currentTimeMillis() - start
      log.info(s"Finished storing CallGraph in $duration ms")
      sender() ! CallGraphStorageActorResponse(artifact, success)
  }

  private def storeCallGraph(artifact: MavenArtifact, cg: CallGraph, project: Project[URL]): Boolean = {
    val session: Session = configuration.graphDatabaseDriver.session()

    val allMethods = cg.reachableMethods().toList

    val externalMethods = new mutable.HashSet[Array[Any]]
    val internalMethods = new mutable.HashSet[Array[Any]]

    for(method <- allMethods){

      val uid = artifact.identifier.toString + ":" + method.toJava

      if(isExternalMethod(method, project)){
        externalMethods.add(Array(artifact.identifier.toString, method.toJava, method.name, uid))
        /*session.run(CallGraphStorageActor.neo4jExternMethodCreationQuery, parameters(
          "art", artifact.identifier.toString,
          "fn", method.toJava,
          "sn", method.name,
          "un", uid
        ))*/
      } else {
        internalMethods.add(Array(artifact.identifier.toString, method.toJava, method.name, uid, if(method.definedMethod.isPublic) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE))
        /*session.run(CallGraphStorageActor.neo4jMethodCreationQuery, parameters(
          "art", artifact.identifier.toString,
          "fn", method.toJava,
          "sn", method.name,
          "un", uid,
          "pub", if(method.definedMethod.isPublic) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE
        ))*/
      }
    }

    session.run("UNWIND $em AS method CREATE (m:Method :ExternMethod {Artifact: method[0], FullName: method[1], SimpleName: method[2], UniqueName: method[3]})",
      parameters("em", externalMethods.toList.asJava))

    session.run("UNWIND $im AS method CREATE (m:Method :LibraryMethod {Artifact: method[0], FullName: method[1], SimpleName: method[2], UniqueName: method[3], IsPublic: method[4]})",
      parameters("im", internalMethods.toList.asJava))


    for(method <- allMethods){
      val sourceUid = s"${artifact.identifier.toString}:${method.toJava}"
      val targetsIdentifiers = cg.calleesOf(method)
        .flatMap(_._2)
        .map(m => s"${artifact.identifier.toString}:${m.toJava}")
        .toList
        .distinct
        .asJava

      session.run("MATCH (m: Method {UniqueName: $un}) UNWIND $targets AS tid MATCH (tm: Method {UniqueName: tid}) CREATE (m)-[:INVOKES]->(tm)",
        parameters("un", sourceUid, "targets", targetsIdentifiers))

    }

    session.close()
    true
  }

  private def isExternalMethod(method: DeclaredMethod, project: Project[URL]): Boolean = {
    method.isInstanceOf[VirtualDeclaredMethod] ||
      !project.allProjectClassFiles.map(_.thisType).contains(method.declaringClassType)
  }
}

case class CallGraphStorageActorResponse(artifact: MavenArtifact,
                                 success: Boolean)

object CallGraphStorageActor {
  def props(config: Configuration): Props = Props(new CallGraphStorageActor(config))

  private val neo4jExternMethodCreationQuery: String =
    "CREATE (m:Method :ExternMethod {Artifact: $art, FullName: $fn, SimpleName: $sn, UniqueName: $un})"

  private val neo4jMethodCreationQuery: String =
    "CREATE (m:Method :LibraryMethod {Artifact: $art, FullName: $fn, SimpleName: $sn, UniqueName: $un, IsPublic: $pub})"


}
