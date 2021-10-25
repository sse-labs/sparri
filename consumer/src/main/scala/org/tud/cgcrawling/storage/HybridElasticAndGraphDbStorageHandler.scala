package org.tud.cgcrawling.storage

import akka.actor.ActorSystem
import com.sksamuel.elastic4s.akka.{AkkaHttpClient, AkkaHttpClientSettings}
import com.sksamuel.elastic4s.{ElasticClient, Response}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.model.LibraryCallGraphEvolution
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.fields.{BooleanField, NestedField, ObjectField, TextField}
import com.sksamuel.elastic4s.requests.indexes.IndexResponse
import org.neo4j.driver.Values.parameters

import scala.collection.JavaConverters.asJavaIterableConverter
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class HybridElasticAndGraphDbStorageHandler(config: Configuration)
                                           (implicit system: ActorSystem) extends StorageHandler {

  private val maxConcurrentEsRequests = 500

  private val isExternFieldName = "IsExtern"
  private val isPublicFieldName = "IsPublic"
  private val nameFieldName = "Name"
  private val signatureFieldName = "Signature"
  private val libraryFieldName = "Library"
  private val releasesFieldName = "Releases"

  private val fullNameFieldName = "FullName"
  private val versionFieldName = "Version"
  private val scopeFieldName = "Scope"
  private val dependenciesFieldName = "Dependencies"

  private val clientProps = AkkaHttpClientSettings(Seq(config.elasticClientUri))

  private val elasticClient: ElasticClient =
    ElasticClient(AkkaHttpClient(clientProps))

  setupIndex()

  override def storeCallGraphEvolution(cgEvolution: LibraryCallGraphEvolution): GraphDbStorageResult = {

    implicit val ec: ExecutionContext = system.dispatcher

    val batch = mutable.Map[String, Future[Response[IndexResponse]]]()
    val responses = mutable.Map[String, Response[IndexResponse]]()

    for(methodEvolution <- cgEvolution.methodEvolutions()){
      batch.put(
        methodEvolution.identifier.fullSignature, elasticClient.execute{
          indexInto(config.elasticMethodIndexName)
            .fields(
              isExternFieldName -> methodEvolution.identifier.isExternal,
              isPublicFieldName-> methodEvolution.identifier.isPublic,
              nameFieldName -> methodEvolution.identifier.simpleName,
              signatureFieldName -> methodEvolution.identifier.fullSignature,
              libraryFieldName -> cgEvolution.libraryName,
              releasesFieldName -> methodEvolution.isActiveIn.toArray
            )
        }
      )

      if(batch.size >= maxConcurrentEsRequests){
        batch
          .mapValues(_.await(duration = 5 minutes))
          .foreach(tuple => responses.put(tuple._1, tuple._2))
        batch.clear()
      }
    }

    if(batch.nonEmpty){
      batch
        .mapValues(_.await(duration = 5 minutes))
        .foreach(tuple => responses.put(tuple._1, tuple._2))
    }


    responses.values.filter(_.isError).foreach(r => log.error("ES ERROR:" + r.body.get))
    var elasticErrorsExist: Boolean = responses.values.count(_.isError) > 0

    val methodElasticIdLookup: Map[String, String] = responses.toMap.mapValues(_.result.id)

    //---STORE METHOD NODES AND INVOCATIONS IN NEO4J
    val session = config.graphDatabaseDriver.session()

    Try{
      val idArray = methodElasticIdLookup.values.toList.asJava
      session.run("UNWIND $ids AS id CREATE (m: Method {ElasticId: id, Library: $lib})",
        parameters("ids", idArray, "lib", cgEvolution.libraryName))

      val invocationData = cgEvolution
        .methodInvocationEvolutions()
        .map{ invocation =>
          Array(methodElasticIdLookup(invocation.invocationIdent.callerIdent.fullSignature),
            methodElasticIdLookup(invocation.invocationIdent.calleeIdent.fullSignature), invocation.isActiveIn.toArray)
        }
        .toList
        .asJava

      session.run("UNWIND $i AS invocation MATCH (caller: Method {ElasticId: invocation[0]}) MATCH (callee: Method {ElasticId: invocation[1]}) CREATE (caller)-[:INVOKES {Releases: invocation[2]}]->(callee)",
        parameters("i", invocationData))
    } match {
      case Success(_) =>
      case Failure(ex) =>
        log.error("Failed to store CG", ex)
        elasticErrorsExist = true
    }

    elasticClient.execute{
      indexInto(config.elasticDependencyIndexName).fields(
        libraryFieldName -> cgEvolution.libraryName,
        releasesFieldName -> cgEvolution.releases(),
        dependenciesFieldName -> cgEvolution.dependencyEvolutions().map{ dep =>
          Map(
            libraryFieldName -> s"${dep.identifier.identifier.groupId}:${dep.identifier.identifier.artifactId}",
            versionFieldName -> dep.identifier.identifier.version,
            releasesFieldName -> dep.isActiveIn,
            scopeFieldName -> dep.identifier.scope
          )
        }
      )
    }

    session.close()

    GraphDbStorageResult(cgEvolution.libraryName, !elasticErrorsExist)
  }

  private def setupIndex(): Unit = {

    def indexPresent(indexName: String): Boolean = elasticClient
      .execute(indexExists(indexName))
      .await
      .toOption
      .map(_.exists)
      .getOrElse(throw new RuntimeException("ElasticSearch not reachable"))

    val methodIndexPresent = indexPresent(config.elasticMethodIndexName)
    val dependencyIndexPresent = indexPresent(config.elasticDependencyIndexName)

    if(!methodIndexPresent){
      log.info("Method index not found, creating it...")
      elasticClient.execute{
        createIndex(config.elasticMethodIndexName)
          .mapping(properties(
            BooleanField(isExternFieldName),
            TextField(nameFieldName),
            TextField(signatureFieldName),
            TextField(libraryFieldName),
            BooleanField(isPublicFieldName),
            TextField(releasesFieldName)
          ))
      }.await
    }

    if(!dependencyIndexPresent){
      log.info("Dependency index not found, creating it...")
      elasticClient.execute{
        createIndex(config.elasticDependencyIndexName)
          .mapping(properties(
            TextField(libraryFieldName),
            TextField(releasesFieldName),
            NestedField(dependenciesFieldName).fields(
              TextField(libraryFieldName),
              TextField(versionFieldName),
              TextField(releasesFieldName),
              TextField(scopeFieldName)
            )
          ))
      }.await
    }
  }


}
