package org.tud.reachablemethods.analysis.dataaccess

import org.neo4j.driver.Values.parameters
import org.neo4j.driver.{AuthTokens, Config, Driver, GraphDatabase, Logging, Session}
import org.tud.reachablemethods.analysis.Configuration

import scala.collection.mutable
import scala.util.Try

class InvocationDataAccessor(config: Configuration) extends DataAccessor(config){

  private[dataaccess] lazy val neo4jDriver: Driver = {
    val c = Config.builder().withLogging(Logging.none()).build()

    val driver = GraphDatabase.driver(config.graphDatabaseUrl,
      AuthTokens.basic(config.graphDatabaseUser, config.graphDatabasePassword), c)

    driver.verifyConnectivity()
    driver
  }

  override def initialize(): Unit = {
    // Will also build the client for the first time, and thus error if creation fails
    if(!requiredIndicesExist()){
      throw new IllegalStateException(s"Missing required ElasticSearch Indices: ${config.elasticMethodIndexName}, ${config.elasticLibraryIndexName}" )
    }
  }

  override def shutdown(): Unit = {
    neo4jDriver.close()
  }

  def getCalleesForMethod(elasticId: String, artifactVersion: String): List[String] = withSession { session =>

    val start: Long = System.currentTimeMillis()
    val result = session.run("MATCH (m: Method {ElasticId: $eid})-[i:INVOKES]->(m2: Method) WHERE $version IN i.Releases RETURN DISTINCT m2.ElasticId AS id",
      parameters("eid", elasticId, "version", artifactVersion))


    val invokedMethodIds = new mutable.HashSet[String]()

    while(result.hasNext){
      invokedMethodIds.add(result.next().get("id").asString())
    }

    val duration = System.currentTimeMillis() - start

    log.debug(s"Callee lookup took $duration ms")

    invokedMethodIds.toList
  }

  private[dataaccess] def requiredIndicesExist(): Boolean = {

    true //TODO: Check for Neo4j constraint
  }

  private def withSession[T](implicit consumer: Session => T): T = {
    val session = neo4jDriver.session()

    val result = Try(consumer(session))

    session.close()

    result.get
  }
}
