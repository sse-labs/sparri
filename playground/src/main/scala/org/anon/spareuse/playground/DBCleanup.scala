package org.anon.spareuse.playground

import org.anon.spareuse.core.storage.postgresql.PostgresDataAccessor
import org.anon.spareuse.mvnem.storage.impl.PostgresStorageAdapter

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object DBCleanup extends App {

  def removeAllAnalysisRuns(aName: String, aVersion: String): Unit = {
    val dbAccess = new PostgresDataAccessor()(ExecutionContext.global)

    dbAccess.getAnalysisRuns(aName, aVersion, includeResults = false, skip = 0, limit = 10000) match {
      case Success(runs) =>
        println(s"Got ${runs.size} runs to delete for analysis $aName:$aVersion")

        runs.foreach{ r =>
          dbAccess.removeAnalysisRun(r.uid) match {
            case Success(_) =>
              println(s"Removed ${r.uid}")
            case Failure(ex) =>
              println(s"Failed to remove ${r.uid}: ${ex.getMessage}")
          }
        }
      case Failure(ex) =>
        ex.printStackTrace
    }
  }

  def removeEntity(): Unit = {
    val adapter = new PostgresStorageAdapter()(ExecutionContext.global)

    adapter.ensureProgramNotPresent("org.bouncycastle:bcprov-jdk15on:1.70")
  }

  removeEntity()

}
