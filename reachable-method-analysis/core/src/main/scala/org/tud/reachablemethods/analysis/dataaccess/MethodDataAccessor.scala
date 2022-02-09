package org.tud.reachablemethods.analysis.dataaccess

import akka.actor.ActorSystem
import com.sksamuel.elastic4s.ElasticApi.{RichFuture, boolQuery, search, searchScroll, termQuery, termsQuery}
import com.sksamuel.elastic4s.{ElasticClient, RequestFailure, RequestSuccess}
import com.sksamuel.elastic4s.ElasticDsl.{IndexExistsHandler, SearchHandler, SearchScrollHandler, indexExists}
import com.sksamuel.elastic4s.akka.{AkkaHttpClient, AkkaHttpClientSettings}
import com.sksamuel.elastic4s.requests.searches.{SearchHit, SearchRequest, SearchResponse}
import org.tud.reachablemethods.analysis.Configuration
import org.tud.reachablemethods.analysis.logging.{AnalysisLogger, AnalysisLogging}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class MethodDataAccessor(config: Configuration, override val log: AnalysisLogger)(implicit system: ActorSystem) extends AnalysisLogging {

  private[dataaccess] lazy val elasticClient: ElasticClient = {
    val props = AkkaHttpClientSettings(Seq(config.elasticClientUri))
    ElasticClient(AkkaHttpClient(props))
  }

  def initialize(): Unit = {
    // Will also build the client for the first time, and thus error if creation fails
    if(!requiredIndicesExist()){
      throw new IllegalStateException(s"Missing required ElasticSearch Indices: ${config.elasticMethodIndexName}, ${config.elasticLibraryIndexName}" )
    }
  }

  def shutdown(): Unit = {
    elasticClient.close()
  }

  def libraryInIndex(libIdent: String): Boolean = {
    elasticClient.execute{
      search(config.elasticLibraryIndexName)
        .query(termQuery(ElasticPropertyNames.libraryKeywordName, libIdent)).size(1).fetchSource(false)
    }.await match {
      case fail: RequestFailure =>
        log.error("Failed to query ElasticSearch: " + fail.error.reason)
        false

      case results: RequestSuccess[SearchResponse] =>
        val numberOfHits = results.result.totalHits
        val time = results.result.took

        log.debug(s"Query for '$libIdent' yielded $numberOfHits hits in $time ms.")

        numberOfHits > 0
    }
  }

  def getIndexedJreVersion: Try[String] = {
    elasticClient.execute {
      search(config.elasticLibraryIndexName)
        .query(termQuery(ElasticPropertyNames.libraryKeywordName, "<none>:<jre>"))
        .size(1)
        .sourceInclude(ElasticPropertyNames.releasesFieldName)
    }.await match {
      case fail: RequestFailure => Failure(fail.error.asException)

      case results: RequestSuccess[SearchResponse] if results.result.totalHits > 0 =>
        Success(results.result.hits.hits.head.sourceAsMap(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]].head)

      case _ =>
        Failure(new IllegalStateException("No JRE in Index"))
    }
  }

  def getArtifactMetadata(libIdent: String, version: String): Try[ArtifactMetadata] = {
    elasticClient.execute{
      search(config.elasticLibraryIndexName)
        .query(termQuery(ElasticPropertyNames.libraryKeywordName, libIdent))
        .size(1)
        .sourceInclude(ElasticPropertyNames.dependenciesFieldName, ElasticPropertyNames.typesFieldName)
    }.await match {
      case fail: RequestFailure => Failure(fail.error.asException)

      case results: RequestSuccess[SearchResponse] =>

        results.result.hits.hits.headOption match {
          case Some(hit) =>
            val libDependencies = hit.sourceAsMap(ElasticPropertyNames.dependenciesFieldName).asInstanceOf[Iterable[Map[String, AnyRef]]]

            val artifactDependencies = libDependencies
              .filter{ obj =>
                val depActiveIn = obj(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]]
                depActiveIn.contains("*") || depActiveIn.contains(version)
              }
              .map{ obj =>
                val lib = obj(ElasticPropertyNames.libraryFieldName).asInstanceOf[String]
                val version = obj(ElasticPropertyNames.versionFieldName).asInstanceOf[String]
                val scope = obj(ElasticPropertyNames.scopeFieldName).asInstanceOf[String]
                ArtifactDependency(lib, version, scope)
              }
              .toList

            val libTypes = hit.sourceAsMap(ElasticPropertyNames.typesFieldName).asInstanceOf[Iterable[Map[String, AnyRef]]]

            val artifactTypes = libTypes
              .filter { obj =>
                val typeActiveIn = obj(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]]
                typeActiveIn.contains("*") || typeActiveIn.contains(version)
              }
              .map { obj =>
                val typeFqn = obj(ElasticPropertyNames.typeFqnFieldName).asInstanceOf[String]

                val typeInstantiatedIn = obj(ElasticPropertyNames.instantiatingReleasesFieldName).asInstanceOf[List[String]]
                val typeInstantiated = typeInstantiatedIn.contains("*") || typeInstantiatedIn.contains(version)

                val typeParents = obj(ElasticPropertyNames.typeParentsFieldName).asInstanceOf[Iterable[Map[String, AnyRef]]]

                val parentOption = typeParents
                  .filter{ parentEntry =>
                    val parentActiveIn = parentEntry(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]]
                    parentActiveIn.contains("*") || parentActiveIn.contains(version)
                  }
                  .map{ parentEntry =>
                    parentEntry(ElasticPropertyNames.typeFqnFieldName).asInstanceOf[String]
                  }
                  .headOption

                val typeInterfaces = obj(ElasticPropertyNames.typeInterfacesFieldName).asInstanceOf[Iterable[Map[String, AnyRef]]]

                val parentInterfaces = typeInterfaces
                  .filter{ parentEntry =>
                    val parentActiveIn = parentEntry(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]]
                    parentActiveIn.contains("*") || parentActiveIn.contains(version)
                  }
                  .map { parentEntry =>
                    parentEntry(ElasticPropertyNames.typeFqnFieldName).asInstanceOf[String]
                  }
                  .toSet


                ElasticTypeData(typeFqn, typeInstantiated, parentOption, parentInterfaces)
              }
              .toList

            Success(ArtifactMetadata(libIdent, version, artifactTypes, artifactDependencies))
          case None =>
            Failure(new IllegalStateException(s"No library found for identifier: $libIdent"))
        }
    }
  }

  def getArtifactMethods(libIdent: String, version: String): Try[List[ElasticMethodData]] = {

    val request = search(config.elasticMethodIndexName).query(boolQuery.must(
        termQuery(ElasticPropertyNames.analyzedLibraryKeywordName, libIdent),
        termQuery(ElasticPropertyNames.releasesKeywordName, version)
      )).sourceInclude(ElasticPropertyNames.methodDataRequiredFields)

    scrollAllResults(request).map(hits => {
      hits.map(hitToMethodData(_, libIdent, version))
    })
  }

  def getArtifactMethodBySignatures(signatures: Iterable[String], libIdent: String, version: String): Try[Iterable[ElasticMethodData]] = {

    if(signatures.size > 10000){Failure(new Exception("Too many signatures"))}

    if(signatures.isEmpty){ Success(List())}
    else {
      elasticClient.execute{
        search(config.elasticMethodIndexName).query(boolQuery.must(
          termsQuery(ElasticPropertyNames.signatureKeywordName, signatures),
          termQuery(ElasticPropertyNames.analyzedLibraryKeywordName, libIdent),
          termQuery(ElasticPropertyNames.releasesKeywordName, version)
        )).sourceInclude(ElasticPropertyNames.methodDataRequiredFields)
      }.await match {
        case fail: RequestFailure =>
          log.error("Failed to query ElasticSearch: " + fail.error.reason)
          Failure(fail.error.asException)

        case results: RequestSuccess[SearchResponse]  =>
          val numberOfHits = results.result.totalHits
          val time = results.result.took
          log.debug(s"Method by ${signatures.size} Signatures query yielded $numberOfHits hits in $time ms.")

          Try {
            results.result.hits.hits.map( hit => hitToMethodData(hit, libIdent, version))
          }
      }
    }

  }

  private[dataaccess] def scrollAllResults(request: SearchRequest): Try[List[SearchHit]] = {

    val batchSize = 10000

    def scrollOn(scrollId: String, batchCnt: Int, totalBatchCount: Int): Try[Array[SearchHit]] = {
      elasticClient.execute{
        searchScroll(scrollId).keepAlive("2m")
      }.await match {
        case fail: RequestFailure => Failure(fail.error.asException)
        case results: RequestSuccess[SearchResponse] =>
          val numberOfHits = results.result.hits.size
          val time = results.result.took
          log.info(s"SCROLLING DATA: $numberOfHits ( $batchCnt / $totalBatchCount ) [$time ms]")
          Success(results.result.hits.hits)
        case _ => Failure(new IllegalStateException("Unknown search response from ElasticSearch"))
      }
    }


    elasticClient.execute{
      request
        .scroll("2m")
        .size(batchSize)
    }.await match {
      case fail: RequestFailure =>
        log.error("Failed to query ElasticSearch: " + fail.error.reason)
        Failure(fail.error.asException)

      case results: RequestSuccess[SearchResponse] =>
        val totalNumberOfHits = results.result.totalHits
        val totalBatchCount = ((totalNumberOfHits / batchSize) + 1).toInt
        val numberOfHits = results.result.hits.size
        val time = results.result.took
        log.info(s"SCROLLING DATA: $numberOfHits ( 1 / $totalBatchCount ) [$time ms]")

        val resultList: mutable.ListBuffer[SearchHit] = new ListBuffer[SearchHit]()

        var hitList = results.result.hits.hits
        resultList.appendAll(hitList)
        var batchCnt = 1
        while(hitList.length == batchSize){
          batchCnt += 1
          hitList = scrollOn(results.result.scrollId.get, batchCnt, totalBatchCount).get
          resultList.appendAll(hitList)
        }

        Success(resultList.toList)
    }
  }


  private[dataaccess] def requiredIndicesExist(): Boolean = {

    def indexPresent(indexName: String): Boolean = elasticClient
      .execute(indexExists(indexName))
      .await
      .toOption
      .map(_.exists)
      .getOrElse(throw new RuntimeException("ElasticSearch not reachable"))

    indexPresent(config.elasticLibraryIndexName) && indexPresent(config.elasticMethodIndexName)
  }

  private[dataaccess] def hitToMethodData(searchHit: SearchHit, libIdent: String, version: String): ElasticMethodData = {
    val fieldMap = searchHit.sourceAsMap
    val mName = fieldMap(ElasticPropertyNames.nameFieldName).asInstanceOf[String]
    val mSig = fieldMap(ElasticPropertyNames.signatureFieldName).asInstanceOf[String]
    val mTypeFqn = fieldMap(ElasticPropertyNames.typeFqnFieldName).asInstanceOf[String]
    val mExtern = fieldMap(ElasticPropertyNames.isExternFieldName).asInstanceOf[Boolean]
    val mObligations = fieldMap(ElasticPropertyNames.obligationFieldName).asInstanceOf[Iterable[Map[String, AnyRef]]]
    val mDefiningLib = fieldMap(ElasticPropertyNames.definingLibraryFieldName).asInstanceOf[String]
    val mCallees = fieldMap(ElasticPropertyNames.calleeFieldName).asInstanceOf[Iterable[Map[String, AnyRef]]]

    val obligations = mObligations
      .filter{ obj =>
        val obligationActiveIn = obj(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]]
        obligationActiveIn.contains("*") || obligationActiveIn.contains(version)
      }
      .map{ obj =>
        InvocationObligation(obj(ElasticPropertyNames.declaredTypeFieldName).asInstanceOf[String],
          obj(ElasticPropertyNames.declaredMethodFieldName).asInstanceOf[String])
      }
      .toList

    val callees = mCallees
      .filter{ obj =>
        val calleeActiveIn = obj(ElasticPropertyNames.releasesFieldName).asInstanceOf[List[String]]
        calleeActiveIn.contains("*") || calleeActiveIn.contains(version)
      }
      .map(obj => obj(ElasticPropertyNames.signatureFieldName).asInstanceOf[String])
      .toList

    ElasticMethodData(mName, mSig, mTypeFqn, mExtern, obligations, callees, mDefiningLib, libIdent, version)
  }

}

private object ElasticPropertyNames {

  val libraryFieldName = "Library"
  val libraryKeywordName = "Library.keyword"

  val analyzedLibraryFieldName = "AnalyzedLibrary"
  val analyzedLibraryKeywordName = "AnalyzedLibrary.keyword"

  val releasesFieldName = "Releases"
  val releasesKeywordName = "Releases.keyword"

  val isExternFieldName = "IsExtern"
  val nameFieldName = "Name"
  val signatureFieldName = "Signature"
  val signatureKeywordName = "Signature.keyword"
  val obligationFieldName = "Obligations"
  val calleeFieldName = "Callees"
  val definingLibraryFieldName = "DefiningLibrary"
  val declaredTypeFieldName = "DeclaredType"
  val declaredMethodFieldName = "DeclaredMethod"
  val dependenciesFieldName = "Dependencies"
  val typesFieldName = "Types"
  val instantiatedTypesFieldName = "InstantiatedTypes"
  val scopeFieldName = "Scope"
  val versionFieldName = "Version"

  val instantiatingReleasesFieldName = "InstantiatingReleases"
  val typeFqnFieldName = "TypeFQN"
  val typeParentsFieldName = "TypeParents"
  val typeInterfacesFieldName = "TypeInterfaces"

  val methodDataRequiredFields = List(
    nameFieldName,
    signatureFieldName,
    typeFqnFieldName,
    isExternFieldName,
    obligationFieldName,
    definingLibraryFieldName,
    calleeFieldName)
}
