package org.anon.spareuse.client.analyses

import org.anon.spareuse.client.http.SparriOracleApiClient
import org.anon.spareuse.execution.analyses.impl.ifds.IFDSTaintFlowSummaryBuilderImpl
import org.anon.spareuse.webapi.model.oracle.TypeNodeRepr
import org.opalj.br.instructions.NEW

import java.io.File
import scala.util.{Failure, Success, Try}

//TODO: Build a return type
class IFDSTaintFlowAnalysis(classesDirectory: File, pomFile: File) extends ClientAnalysis[Int](classesDirectory, pomFile) {

  private val remoteAnalysisName: String = IFDSTaintFlowSummaryBuilderImpl.analysisName
  private val remoteAnalysisVersion: String = "0.0.1"

  private val oracleApiClient: SparriOracleApiClient = new SparriOracleApiClient

  override def close(): Unit = {
    super.close()
    oracleApiClient.close()
  }

  override protected[analyses] def requirements: Seq[AnalysisRequirement] =  Seq(AnalysisRequirement("org.apache.maven.indexer:indexer-core!org.apache.maven.indexer:indexer-core:6.0.0", remoteAnalysisName, remoteAnalysisVersion),
    AnalysisRequirement("ch.qos.logback:logback-classic!ch.qos.logback:logback-classic:1.4.4", remoteAnalysisName, remoteAnalysisVersion)) //TODO: Use real code once dependency extraction works
    /*getAllDependencies
      .map(dep => AnalysisRequirement(dep.identifier.toString, remoteAnalysisName, remoteAnalysisVersion))
      .toSeq*/

  override def execute(): Try[Int] = Try {
    val p = getOpalProject(loadJre = false)

    val fakeDependencies = Set("ch.qos.logback:logback-classic:1.4.4", "org.apache.maven.indexer:indexer-core:6.0.0")

    val typeNodeRepresentations = p
      .allProjectClassFiles
      .map{ cf => TypeNodeRepr(cf.fqn, cf.superclassType.map(_.fqn), cf.interfaceTypes.map(_.fqn).toSet, cf.isInterfaceDeclaration) }
      .toSet

    val allTypesInitialized = p
      .allProjectClassFiles
      .flatMap(_.methodsWithBody)
      .flatMap(_.body.get.instructions)
      .filter(_.isInstanceOf[NEW])
      .map(_.asNEW.objectType.fqn)
      .toSet

    Try(oracleApiClient.startOracleSession(fakeDependencies, typeNodeRepresentations, allTypesInitialized, 0, None)) match {
      case Success(_) =>
        log.info(s"Successfully started resolution session with server, session-id = ${oracleApiClient.getToken.getOrElse("<NONE>")}")

        //TODO: Manage session lifecycle

        0
      case Failure(ex) =>
        log.error(s"Failed to start oracle session", ex)
        throw ex
    }

  }
}
