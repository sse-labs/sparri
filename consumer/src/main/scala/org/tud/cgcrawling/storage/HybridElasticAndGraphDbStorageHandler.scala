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
  private val descriptorFieldName = "Descriptor"
  private val signatureFieldName = "Signature"
  private val libraryFieldName = "Library"
  private val libraryKeywordName = "Library.keyword"
  private val analyzedLibraryFieldName = "AnalyzedLibrary"
  private val definingLibraryFieldName = "DefiningLibrary"
  private val releasesFieldName = "Releases"
  private val obligationFieldName = "Obligations"
  private val calleeFieldName = "Callees"
  private val declaredTypeFieldName = "DeclaredType"
  private val declaredMethodFieldName = "DeclaredMethod"

  private val versionFieldName = "Version"
  private val scopeFieldName = "Scope"
  private val dependenciesFieldName = "Dependencies"
  private val typesFieldName = "Types"
  private val instantiatingReleasesFieldName = "InstantiatingReleases"
  private val typeFqnFieldName = "TypeFQN"
  private val typeParentsFieldName = "TypeParents"
  private val typeInterfacesFieldName = "TypeInterfaces"

  private val clientProps = AkkaHttpClientSettings(Seq(config.elasticClientUri))

  private val elasticClient: ElasticClient =
    ElasticClient(AkkaHttpClient(clientProps))

  setupIndex()

  override def storeCallGraphEvolution(cgEvolution: LibraryCallGraphEvolution): GraphDbStorageResult = {

    implicit val ec: ExecutionContext = system.dispatcher

    log.info("Starting to store data in ES ...")

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
        //TODO: Can we leave out some non-project types? Maybe non-instantiated leaf nodes in hierarchy, as they must be included
        //TODO: in the project extending the current one.
        typesFieldName -> cgEvolution.typeEvolutions().map(tEvo => Map(
          typeFqnFieldName -> tEvo.typeFqn,
          releasesFieldName -> buildReleasesValue(tEvo.isActiveIn, libReleases), // "Releases" will be "*" if they are equal to the Lib's Releases
          instantiatingReleasesFieldName -> buildReleasesValue(tEvo.isInstantiatedIn, libReleases), // "InstantiatingReleases" will be "*" if equal to Lib's Releases
          typeParentsFieldName -> tEvo.parentTypeFqnToReleasesMap.map { entry => //TODO: Make java/lang/Object implicit?
            Map(typeFqnFieldName -> entry._1, releasesFieldName -> buildReleasesValue(entry._2.toList, libReleases))
          },
          typeInterfacesFieldName -> tEvo.parentInterfaceFqnToReleasesMap.map { entry =>
            Map(typeFqnFieldName -> entry._1, releasesFieldName -> buildReleasesValue(entry._2.toList, libReleases))
          }
        ))
      )
    }.await

    if(elasticLibResponse.isError) {
      log.error("Failed to store library in ES", elasticLibResponse.error.asException)
      GraphDbStorageResult(cgEvolution.libraryName, success = false)
    } else {

      log.info("Starting to store methods...")

      val elasticErrorsForMethods = cgEvolution
        .methodEvolutions()
        .toList
        .grouped(maxConcurrentEsRequests)
        .map { batch =>
          elasticClient.execute {
            bulk(
              batch.map { methodEvolution =>

                val methodReleases = methodEvolution.isActiveIn

                indexInto(config.elasticMethodIndexName)
                  .fields(
                    isExternFieldName -> methodEvolution.identifier.isExternal,
                    isPublicFieldName -> methodEvolution.identifier.isPublic,
                    nameFieldName -> methodEvolution.identifier.simpleName,
                    descriptorFieldName -> methodEvolution.identifier.methodDescriptor,
                    signatureFieldName -> methodEvolution.identifier.fullSignature,
                    analyzedLibraryFieldName -> cgEvolution.libraryName,
                    definingLibraryFieldName -> methodEvolution.identifier.definingLibraryName,
                    releasesFieldName -> methodReleases.toArray,
                    typeFqnFieldName -> methodEvolution.identifier.typeFqn,
                    obligationFieldName -> methodEvolution.obligationEvolutions().map( oEvo => Map(
                      declaredTypeFieldName -> oEvo.invocationObligation.declaredTypeName,
                      declaredMethodFieldName -> (oEvo.invocationObligation.methodName + oEvo.invocationObligation.descriptor),
                      releasesFieldName -> buildReleasesValue(oEvo.isActiveIn, methodReleases)
                    )),
                    calleeFieldName -> cgEvolution.calleeEvolutionsAt(methodEvolution.identifier)
                      .map( iEvo => Map(
                        signatureFieldName -> iEvo.invocationIdent.calleeIdent.fullSignature,
                        releasesFieldName -> buildReleasesValue(iEvo.isActiveIn, methodReleases)
                      ))
                  )
              }
            )
          }.await(10 minutes)
        }
        .map(res => {
          if(res.isError){
            log.error("Bulk response failed: " + res.error.reason, res.error.asException)
          }

          res.result.failures.foreach( i => log.error("Bulk item failed: " + i.error.map(_.reason).getOrElse("<No Error>")))
          res
        })
        .exists(res => res.isError || res.result.hasFailures)

      if(elasticErrorsForMethods) log.error("Got errors while storing methods in ES.")



      log.info("Finished storing data in ES.")



      GraphDbStorageResult(cgEvolution.libraryName, !elasticErrorsForMethods)
    }
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
        .query(termQuery(libraryKeywordName, libIdent)).size(1).fetchSource(false)
    }.await match {
      case fail: RequestFailure =>
        log.error("Failed to query ElasticSearch: " + fail.error.reason)
        None

      case results: RequestSuccess[SearchResponse] =>
        val numberOfHits = results.result.totalHits
        val time = results.result.took

        log.debug(s"Query for '$libIdent' yielded $numberOfHits hits in $time ms.")

        Some(numberOfHits > 0)
    }
  }

  private def setupIndex(): Unit = {

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
          .settings(Map("index.mapping.nested_objects.limit" -> 50000))
          .mapping(properties(
            BooleanField(isExternFieldName),
            KeywordField(nameFieldName),
            TextField(descriptorFieldName),
            //TODO: Signature might be redundant now that we have TypeFqn, SimpleName and Descriptor
            KeywordField(signatureFieldName),
            KeywordField(analyzedLibraryFieldName),
            KeywordField(definingLibraryFieldName),
            BooleanField(isPublicFieldName),
            KeywordField(releasesFieldName),
            KeywordField(typeFqnFieldName),
            NestedField(calleeFieldName).fields(
              TextField(signatureFieldName),
              KeywordField(releasesFieldName)
            ),
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
          .settings(Map("index.mapping.nested_objects.limit" -> 50000))
          .mapping(properties(
            KeywordField(libraryFieldName),
            KeywordField(releasesFieldName),
            NestedField(dependenciesFieldName).fields(
              KeywordField(libraryFieldName),
              KeywordField(versionFieldName),
              KeywordField(releasesFieldName),
              KeywordField(scopeFieldName)
            ),
            NestedField(typesFieldName).fields(
              KeywordField(typeFqnFieldName),
              KeywordField(releasesFieldName),
              KeywordField(instantiatingReleasesFieldName),
              NestedField(typeParentsFieldName).fields(
                KeywordField(typeFqnFieldName),
                KeywordField(releasesFieldName)
              ),
              NestedField(typeInterfacesFieldName).fields(
                KeywordField(typeFqnFieldName),
                KeywordField(releasesFieldName)
              )
            )
          ))
      }.await
    }
  }
}
