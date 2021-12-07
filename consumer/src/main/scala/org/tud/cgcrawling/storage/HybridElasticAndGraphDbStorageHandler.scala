package org.tud.cgcrawling.storage

import akka.actor.ActorSystem
import com.sksamuel.elastic4s.ElasticApi.{search, termQuery}
import com.sksamuel.elastic4s.akka.{AkkaHttpClient, AkkaHttpClientSettings}
import com.sksamuel.elastic4s.{ElasticClient, RequestFailure, RequestSuccess}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.model.{LibraryCallGraphEvolution, MethodIdentifier}
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.fields.{BooleanField, KeywordField, NestedField, TextField}
import com.sksamuel.elastic4s.requests.searches.SearchResponse
import org.neo4j.driver.Session
import org.neo4j.driver.Values.parameters

import scala.collection.JavaConverters.asJavaIterableConverter
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class HybridElasticAndGraphDbStorageHandler(config: Configuration)
                                           (implicit system: ActorSystem) extends StorageHandler {

  private val maxConcurrentEsRequests = 30
  private val neo4jMethodInsertBatchSize = 400
  private val neo4jMethodInvocationInsertBatchSize = 200

  private val isExternFieldName = "IsExtern"
  private val isPublicFieldName = "IsPublic"
  private val nameFieldName = "Name"
  private val signatureFieldName = "Signature"
  private val libraryFieldName = "Library"
  private val analyzedLibraryFieldName = "AnalyzedLibrary"
  private val definingLibraryFieldName = "DefiningLibrary"
  private val releasesFieldName = "Releases"
  private val isJreFieldName = "IsJRE"
  private val obligationFieldName = "Obligations"
  private val declaredTypeFieldName = "DeclaredType"
  private val declaredMethodFieldName = "DeclaredMethod"

  private val versionFieldName = "Version"
  private val scopeFieldName = "Scope"
  private val dependenciesFieldName = "Dependencies"
  private val instantiatedTypesFieldName = "InstantiatedTypes"

  private val clientProps = AkkaHttpClientSettings(Seq(config.elasticClientUri))

  private val elasticClient: ElasticClient =
    ElasticClient(AkkaHttpClient(clientProps))

  setupIndex()

  override def storeCallGraphEvolution(cgEvolution: LibraryCallGraphEvolution): GraphDbStorageResult = {

    implicit val ec: ExecutionContext = system.dispatcher

    val idLookup: mutable.Map[MethodIdentifier, String] = new mutable.HashMap()

    log.info("Starting to store data in ES ...")

    val elasticErrorsForMethods = cgEvolution
      .methodEvolutions()
      .toList
      .grouped(maxConcurrentEsRequests)
      .map { batch =>
        (batch, elasticClient.execute {
          bulk(
            batch.map { methodEvolution =>

              val methodReleases = methodEvolution.isActiveIn
              // If defining Lib is not explicit and we are external => JRE call
              val definingLib = methodEvolution.identifier.definingArtifact.map(i => s"${i.groupId}:${i.artifactId}").getOrElse{
                if(methodEvolution.identifier.isJREMethod) "<none>:<jre>"
                else "<unknown>:<unknown>"
              }

              indexInto(config.elasticMethodIndexName)
                .fields(
                  isExternFieldName -> methodEvolution.identifier.isExternal,
                  isPublicFieldName -> methodEvolution.identifier.isPublic,
                  nameFieldName -> methodEvolution.identifier.simpleName,
                  signatureFieldName -> methodEvolution.identifier.fullSignature,
                  analyzedLibraryFieldName -> cgEvolution.libraryName,
                  definingLibraryFieldName -> definingLib,
                  releasesFieldName -> methodReleases.toArray,
                  isJreFieldName -> methodEvolution.identifier.isJREMethod,
                  obligationFieldName -> methodEvolution.obligationEvolutions().map( oEvo => Map(
                    declaredTypeFieldName -> oEvo.invocationObligation.declaredTypeName,
                    declaredMethodFieldName -> (oEvo.invocationObligation.methodName + oEvo.invocationObligation.descriptor),
                    releasesFieldName -> buildReleasesValue(oEvo.isActiveIn, methodReleases)
                  ))
                )
            }
          )
        }.await(10 minutes))
      }
      .map{ tuple =>

        if(!tuple._2.isError && !tuple._2.result.hasFailures){
          val idList = tuple._2.result.items.toList.map(_.id)

          for (i <- idList.indices){
            val ident = tuple._1(i).identifier
            val id = idList(i)
            idLookup.put(ident, id)
          }
        }

        tuple._2
      }
      .exists(res => res.isError || res.result.hasFailures)

    if(elasticErrorsForMethods) log.error("Got errors while storing methods in ES.")

    val elasticLibResponse = elasticClient.execute{

        val libReleases = cgEvolution.releases()

        indexInto(config.elasticLibraryIndexName).fields(
          libraryFieldName -> cgEvolution.libraryName,
          releasesFieldName -> libReleases.toArray,
          dependenciesFieldName -> cgEvolution.dependencyEvolutions().map{ dep =>
            Map(
              libraryFieldName -> s"${dep.identifier.identifier.groupId}:${dep.identifier.identifier.artifactId}",
              versionFieldName -> dep.identifier.identifier.version,
              releasesFieldName -> buildReleasesValue(dep.isActiveIn, libReleases),
              scopeFieldName -> dep.identifier.scope
            )
          },
          instantiatedTypesFieldName -> cgEvolution.instantiatedTypeEvolutions().map( tEvo => Map(
            declaredTypeFieldName -> tEvo._1,
            releasesFieldName -> buildReleasesValue(tEvo._2, libReleases)
          ))
        )
      }.await

    if(elasticLibResponse.isError){
      log.error("Failed to store library in ES", elasticLibResponse.error.asException)
      println(elasticLibResponse.error.reason)
    }

    val elasticErrorsForLibrary = elasticLibResponse.isError

    if(elasticErrorsForLibrary) log.error("Got error while storing library in ES.")

    log.info("Finished storing data in ES.")


    log.info("Starting to store method relations in Neo4j..")

    //---STORE METHOD NODES AND INVOCATIONS IN NEO4J
    val session = config.graphDatabaseDriver.session()

    val neo4jErrorsExist = Try{
      cgEvolution
        .methodEvolutions()
        .map(evo => idLookup(evo.identifier))
        .grouped(neo4jMethodInsertBatchSize)
        .foreach{ batch =>
          session.run("UNWIND $ids AS id CREATE (m: Method {ElasticId: id, Library: $lib})",
            parameters("ids", batch.toList.asJava, "lib", cgEvolution.libraryName))
        }

      cgEvolution
        .methodInvocationEvolutions()
        .map{ invocation =>
          Array(idLookup(invocation.invocationIdent.callerIdent),
            idLookup(invocation.invocationIdent.calleeIdent), invocation.isActiveIn.toArray)
        }
        .grouped(neo4jMethodInvocationInsertBatchSize)
        .foreach{ batch =>
          session.run("UNWIND $i AS invocation MATCH (caller: Method {ElasticId: invocation[0]}) MATCH (callee: Method {ElasticId: invocation[1]}) CREATE (caller)-[:INVOKES {Releases: invocation[2]}]->(callee)",
            parameters("i", batch.toList.asJava))
        }
    } match {
      case Success(_) =>
        false
      case Failure(ex) =>
        log.error("Failed to store CG", ex)
        true
    }
    session.close()

    log.info("Finished storing method relations.")

    GraphDbStorageResult(cgEvolution.libraryName, !elasticErrorsForLibrary && !elasticErrorsForMethods && !neo4jErrorsExist)
  }

  private def buildReleasesValue(activeReleases: List[String], parentActiveReleases: List[String]): Array[String]= {
    if(activeReleases.size == parentActiveReleases.size){
      Array("*")
    } else {
      activeReleases.toArray
    }
  }

  override def libraryExists(libIdent: String): Option[Boolean] = {


    elasticClient.execute{
      search(config.elasticLibraryIndexName)
        .query(termQuery(libraryFieldName, libIdent)).size(1).fetchSource(false)
    }.await match {
      case fail: RequestFailure =>
        log.error("Failed to query ElasticSearch: ", fail.error.reason)
        None

      case results: RequestSuccess[SearchResponse] =>
        val numberOfHits = results.result.totalHits
        val time = results.result.took

        log.debug(s"Query for '$libIdent' yielded $numberOfHits hits in $time ms.")

        Some(numberOfHits > 0)

      case response: RequestSuccess[_] =>
        log.error(s"Unknown response type: ${response.result}")
        None
    }
  }

  private def setupIndex(): Unit = {

    val session: Session = config.graphDatabaseDriver.session()

    Try{
      session.run("CREATE CONSTRAINT unique_es IF NOT EXISTS ON (m:Method) ASSERT m.ElasticId IS UNIQUE")
    } match {
      case Failure(ex) =>
        log.error("Error while ensuring Neo4j constraints are present!", ex)
      case _ =>
        log.info("Neo4j indices are present.")
    }

    session.close()

    def indexPresent(indexName: String): Boolean = elasticClient
      .execute(indexExists(indexName))
      .await
      .toOption
      .map(_.exists)
      .getOrElse(throw new RuntimeException("ElasticSearch not reachable"))

    val methodIndexPresent = indexPresent(config.elasticMethodIndexName)
    val libraryIndexPresent = indexPresent(config.elasticLibraryIndexName)

    if(!methodIndexPresent){
      log.info("Method index not found, creating it...")
      elasticClient.execute{
        createIndex(config.elasticMethodIndexName)
          .mapping(properties(
            BooleanField(isExternFieldName),
            TextField(nameFieldName),
            TextField(signatureFieldName),
            KeywordField(analyzedLibraryFieldName),
            KeywordField(definingLibraryFieldName),
            BooleanField(isPublicFieldName),
            KeywordField(releasesFieldName),
            NestedField(obligationFieldName).fields(
              TextField(declaredTypeFieldName),
              TextField(declaredMethodFieldName),
              KeywordField(releasesFieldName)
            )
          ))
      }.await
    }

    if(!libraryIndexPresent){
      log.info("Library index not found, creating it...")
      elasticClient.execute{
        createIndex(config.elasticLibraryIndexName)
          .mapping(properties(
            KeywordField(libraryFieldName),
            KeywordField(releasesFieldName),
            NestedField(dependenciesFieldName).fields(
              KeywordField(libraryFieldName),
              KeywordField(versionFieldName),
              KeywordField(releasesFieldName),
              KeywordField(scopeFieldName)
            ),
            NestedField(instantiatedTypesFieldName).fields(
              KeywordField(declaredTypeFieldName),
              KeywordField(releasesFieldName)
            )
          ))
      }.await
    }
  }
}
