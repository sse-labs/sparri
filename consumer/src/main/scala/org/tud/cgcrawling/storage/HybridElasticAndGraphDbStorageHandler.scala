package org.tud.cgcrawling.storage

import akka.actor.ActorSystem
import com.sksamuel.elastic4s.akka.{AkkaHttpClient, AkkaHttpClientSettings}
import com.sksamuel.elastic4s.ElasticClient
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.model.LibraryCallGraphEvolution
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.fields.{BooleanField, TextField}
import org.neo4j.driver.Values.parameters

import scala.collection.JavaConverters.asJavaIterableConverter
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class HybridElasticAndGraphDbStorageHandler(config: Configuration)
                                           (implicit system: ActorSystem) extends StorageHandler {

  private val isExternFieldName = "IsExtern"
  private val isPublicFieldName = "IsPublic"
  private val nameFieldName = "Name"
  private val signatureFieldName = "Signature"
  private val libraryFieldName = "Library"
  private val releasesFieldName = "Releases"

  private val clientProps = AkkaHttpClientSettings(Seq(config.elasticClientUri))

  private val elasticClient: ElasticClient =
    ElasticClient(AkkaHttpClient(clientProps))

  setupIndex()

  override def storeCallGraphEvolution(cgEvolution: LibraryCallGraphEvolution): GraphDbStorageResult = {

    implicit val ec: ExecutionContext = system.dispatcher

    //---STORE ALL METHOD INFORMATION IN ES---
    val storageFutures = cgEvolution
      .methodEvolutions()
      .map{ methodEvolution =>
        (methodEvolution.identifier.fullSignature, elasticClient.execute{
          indexInto(config.elasticMethodIndexName)
            .fields(
              isExternFieldName -> methodEvolution.identifier.isExternal,
              isPublicFieldName-> methodEvolution.identifier.isPublic,
              nameFieldName -> methodEvolution.identifier.simpleName,
              signatureFieldName -> methodEvolution.identifier.fullSignature,
              libraryFieldName -> cgEvolution.libraryName,
              releasesFieldName -> methodEvolution.isActiveIn.toArray
            )
        })
      }
      .toMap

    val storageResponses = storageFutures
      .mapValues(_.await(5 minutes))

    storageResponses.values.filter(_.isError).foreach(r => log.error("ES ERROR:" + r.body.get))
    var elasticErrorsExist: Boolean = storageResponses.values.count(_.isError) > 0

    val methodElasticIdLookup: Map[String, String] = storageResponses.mapValues(_.result.id)

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

    session.close()

    GraphDbStorageResult(cgEvolution.libraryName, !elasticErrorsExist)
  }

  private def setupIndex(): Unit = {
    val indexPresent: Boolean = elasticClient
      .execute(indexExists(config.elasticMethodIndexName))
      .await
      .toOption
      .map(_.exists)
      .getOrElse(throw new RuntimeException("ElasticSearch not reachable!"))

    if(!indexPresent){
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
      }
    }
  }


}
