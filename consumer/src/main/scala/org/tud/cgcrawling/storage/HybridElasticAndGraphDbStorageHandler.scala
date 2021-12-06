package org.tud.cgcrawling.storage

import akka.actor.ActorSystem
import com.sksamuel.elastic4s.akka.{AkkaHttpClient, AkkaHttpClientSettings}
import com.sksamuel.elastic4s.ElasticClient
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.model.{LibraryCallGraphEvolution, MethodIdentifier}
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.fields.{BooleanField, NestedField, TextField}
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

  private val maxConcurrentEsRequests = 10
  private val neo4jMethodInsertBatchSize = 200
  private val neo4jMethodInvocationInsertBatchSize = 100

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

    val elasticErrorsExist = cgEvolution
      .methodEvolutions()
      .toList
      .grouped(maxConcurrentEsRequests)
      .map { batch =>
        (batch, elasticClient.execute {
          bulk(
            batch.map { methodEvolution =>
              indexInto(config.elasticMethodIndexName)
                .fields(
                  isExternFieldName -> methodEvolution.identifier.isExternal,
                  isPublicFieldName -> methodEvolution.identifier.isPublic,
                  nameFieldName -> methodEvolution.identifier.simpleName,
                  signatureFieldName -> methodEvolution.identifier.fullSignature,
                  analyzedLibraryFieldName -> cgEvolution.libraryName,
                  definingLibraryFieldName -> methodEvolution.identifier.definingArtifact.map(i => s"${i.groupId}:${i.artifactId}").getOrElse("<none>"),
                  releasesFieldName -> methodEvolution.isActiveIn.toArray,
                  isJreFieldName -> methodEvolution.identifier.isJREMethod,
                  obligationFieldName -> methodEvolution.obligationEvolutions().map( oEvo => Map(
                    declaredTypeFieldName -> oEvo.invocationObligation.declaredTypeName,
                    declaredMethodFieldName -> (oEvo.invocationObligation.methodName + oEvo.invocationObligation.descriptor),
                    releasesFieldName -> oEvo.isActiveIn.toArray
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
      .exists(res => res.isError || res.result.hasFailures) ||
      elasticClient.execute{
        indexInto(config.elasticLibraryIndexName).fields(
          libraryFieldName -> cgEvolution.libraryName,
          releasesFieldName -> cgEvolution.releases(),
          dependenciesFieldName -> cgEvolution.dependencyEvolutions().map{ dep =>
            Map(
              libraryFieldName -> s"${dep.identifier.identifier.groupId}:${dep.identifier.identifier.artifactId}",
              versionFieldName -> dep.identifier.identifier.version,
              releasesFieldName -> dep.isActiveIn,
              scopeFieldName -> dep.identifier.scope
            )
          },
          instantiatedTypesFieldName -> cgEvolution.instantiatedTypeEvolutions().map( tEvo => Map(
            declaredTypeFieldName -> tEvo._1,
            releasesFieldName -> tEvo._2.toArray
          ))
        )
      }.await.isError

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

    GraphDbStorageResult(cgEvolution.libraryName, !elasticErrorsExist && !neo4jErrorsExist)
  }

  override def libraryExists(libName: String): Option[Boolean] = {
    val session = config.graphDatabaseDriver.session()

    val existsTry = Try {
      session
        .run("MATCH (m:Method {Library: $lib}) RETURN COUNT(m) AS cnt", parameters("lib", libName))
        .single()
        .get("cnt")
        .asInt() > 0
    }

    session.close()



    existsTry.toOption
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
            TextField(analyzedLibraryFieldName),
            TextField(definingLibraryFieldName),
            BooleanField(isPublicFieldName),
            TextField(releasesFieldName),
            NestedField(obligationFieldName).fields(
              TextField(declaredTypeFieldName),
              TextField(declaredMethodFieldName),
              TextField(releasesFieldName)
            )
          ))
      }.await
    }

    if(!libraryIndexPresent){
      log.info("Library index not found, creating it...")
      elasticClient.execute{
        createIndex(config.elasticLibraryIndexName)
          .mapping(properties(
            TextField(libraryFieldName),
            TextField(releasesFieldName),
            NestedField(dependenciesFieldName).fields(
              TextField(libraryFieldName),
              TextField(versionFieldName),
              TextField(releasesFieldName),
              TextField(scopeFieldName)
            ),
            NestedField(instantiatedTypesFieldName).fields(
              TextField(declaredTypeFieldName),
              TextField(releasesFieldName)
            )
          ))
      }.await
    }
  }
}
