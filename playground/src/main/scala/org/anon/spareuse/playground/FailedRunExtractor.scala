package org.anon.spareuse.playground

import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.RunState
import org.anon.spareuse.core.storage.postgresql.{PostgresAnalysisTables, PostgresEntityTables, SoftwareAnalysisRunRepr}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.DurationInt

object FailedRunExtractor extends PostgresAnalysisTables with PostgresEntityTables {

  private final val db = Database.forConfig("spa-reuse.postgres")

  private def getFailedRunIds: Set[Long] = {
    val resultFuture = db.run(analysisRunsTable.filter(run => run.state === RunState.Failed.id).map(_.id).result)

    Await.result(resultFuture, 10.minutes).toSet
  }

  private def getRunInputNames(runIds: Set[Long]): Set[String] = {
    val resultFuture = db
      .run(analysisRunInputsTable.filter(ri => ri.analysisRunID inSet runIds).map(_.inputEntityID).result)
      .flatMap{ entityIds =>
        db.run(entitiesTable.filter(_.id inSet entityIds).map(_.name).result)
      }(ExecutionContext.global)

    Await.result(resultFuture, 10.minutes).toSet
  }

  def main(args: Array[String]): Unit = {
    val runs = getFailedRunIds

    val inputNames = getRunInputNames(runs)

    val libraries = inputNames
      .map(MavenIdentifier.fromGAV)
      .filter(_.isDefined)
      .map(_.get.toGA)
      .map { libName =>
        (libName, inputNames.count(_.startsWith(libName)))
      }

    println(s"Got ${libraries.size} failed libraries:")
    libraries.foreach(l => println(s"\t - ${l._1} (${l._2} times)"))
  }

}
