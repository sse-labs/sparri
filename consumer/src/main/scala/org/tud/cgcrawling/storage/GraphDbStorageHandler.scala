package org.tud.cgcrawling.storage

import org.neo4j.driver.Values.parameters
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.model.{LibraryCallGraphEvolution, MethodIdentifier}

import scala.collection.JavaConverters.asJavaIterableConverter
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class GraphDbStorageHandler(configuration: Configuration) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def storeCallGraphEvolution(cgEvolution: LibraryCallGraphEvolution): GraphDbStorageResult = {
    val start = System.currentTimeMillis()
    log.info(s"Storing CallGraph for library ${cgEvolution.libraryName}")

    Try(storeCgEvolution(cgEvolution)) match {
      case Success(_) =>
        val duration = System.currentTimeMillis() - start
        log.info(s"Finished storing CallGraph in $duration ms")
        GraphDbStorageResult(cgEvolution.libraryName, success = true)
      case Failure(ex) =>
        log.error(s"Failed to store callgraph for library ${cgEvolution.libraryName}", ex)
        GraphDbStorageResult(cgEvolution.libraryName, success = false)
    }
  }

  private def storeCgEvolution(cgEvolution: LibraryCallGraphEvolution): Unit = {
    val libIdent = cgEvolution.libraryName

    def getMethodUid(identifier: MethodIdentifier) = {
      if(identifier.isExternal)
        s"$libIdent:${identifier.fullSignature}--EXTERN"
      else
        s"$libIdent:${identifier.fullSignature}"
    }

    val allMethodData = new mutable.HashSet[Array[Any]]

    for(methodEvolution <- cgEvolution.methodEvolutions()){
      val uid = getMethodUid(methodEvolution.identifier)

      val name = methodEvolution.identifier.simpleName
      val releases = methodEvolution.isActiveIn.toArray
      val signature = methodEvolution.identifier.fullSignature
      val isExternal = methodEvolution.identifier.isExternal

      allMethodData.add(Array(libIdent, uid, name, signature, releases, isExternal))
    }

    val allInvocationData = new mutable.HashSet[Array[Any]]

    for(invocationEvolution <- cgEvolution.methodInvocationEvolutions()){
      val callerUid = getMethodUid(invocationEvolution.invocationIdent.callerIdent)
      val calleeUid = getMethodUid(invocationEvolution.invocationIdent.calleeIdent)
      val releases = invocationEvolution.isActiveIn.toArray

      allInvocationData.add(Array(callerUid, calleeUid, releases))
    }

    val allDependenciesData = cgEvolution.dependencyEvolutions()
      .map{ depEvo =>
        Array(depEvo.identifier.identifier.toString,
          s"${depEvo.identifier.identifier.groupId}:${depEvo.identifier.identifier.artifactId}",
          depEvo.identifier.identifier.version,
          depEvo.identifier.scope,
          depEvo.isActiveIn.toArray)
      }
      .toList
      .asJava

    val session = configuration.graphDatabaseDriver.session()

    session.run("UNWIND $m AS method CREATE (m:Method {Library: method[0], UniqueName: method[1], SimpleName: method[2], Signature: method[3], Releases: method[4], IsExtern: method[5]})",
      parameters("m", allMethodData.toList.asJava))

    session.run("UNWIND $i AS invocation MATCH (caller: Method {UniqueName: invocation[0]}) MATCH (callee: Method {UniqueName: invocation[1]}) CREATE (caller)-[:INVOKES {Releases: invocation[2]}]->(callee)",
      parameters("i", allInvocationData.toList.asJava))

    session.run("CREATE (l: MavenLibrary{LibraryId: $lid, GroupId: $gid, ArtifactId: $aid, Releases: $r})",
      parameters("lid", cgEvolution.libraryName ,"gid", cgEvolution.groupId, "aid", cgEvolution.artifactId, "r", cgEvolution.releases().toArray))

    session.run("MATCH (l: MavenLibrary {LibraryId: $lid}) UNWIND $d AS dependency " +
      "MERGE (d: MavenDependency{FullName: dependency[0], LibraryId: dependency[1], Version: dependency[2]}) " +
      "CREATE (l)-[:DEPENDS_ON {Scope: dependency[3], Releases: dependency[4]}]->(d)",
      parameters("lid", cgEvolution.libraryName, "d", allDependenciesData))

    session.close()
  }

}

case class GraphDbStorageResult(libraryName: String, success: Boolean)
