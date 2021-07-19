package org.tud.cgcrawling.storage

import org.neo4j.driver.Session
import org.neo4j.driver.Values.parameters
import org.tud.cgcrawling.Configuration


trait CallGraphStorageQueries {
  val configuration: Configuration

  def artifactGAVExistsInDb(gav: String): Boolean = withSession { session =>

    session.run("MATCH (m: Method {Artifact: $gav}) RETURN COUNT(m) AS cnt",
      parameters("gav", gav))
      .single()
      .get("cnt")
      .asInt() > 0
  }

  private def withSession[T](implicit theFunction: Session => T): T = {
    val session = configuration.graphDatabaseDriver.session()

    val result: T = theFunction.apply(session)

    session.close()

    result
  }
}
