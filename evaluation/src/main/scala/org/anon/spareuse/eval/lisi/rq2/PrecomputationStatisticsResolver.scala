package org.anon.spareuse.eval.lisi.rq2

import org.anon.spareuse.core.model.RunState
import org.anon.spareuse.core.storage.postgresql.PostgresDataAccessor
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IterableHasAsJava}
import scala.util.{Failure, Success, Try}

object PrecomputationStatisticsResolver {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private final val dataAccessor: PostgresDataAccessor =
    new PostgresDataAccessor()(ExecutionContext.global)

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      log.error("Missing required argument(s): GA List File or Output Directory")
      log.error("Usage: SummaryGenerationInitiator <ga-list-file> <out-dir>")
      System.exit(-1)
    }

    Try(readFromFile(args(0))) match {
      case Success(gaList) =>
        log.info(s"Found ${gaList.size} GAs at ${args(0)}")
        val outDir = Paths.get(args(1))

        if(!outDir.toFile.exists() || !outDir.toFile.isDirectory)
          throw new IllegalStateException(s"Invalid output directory specified: ${args(1)}")

        gaList.foreach{ ga =>
          log.info(s"Gathering statistics for library $ga ...")
          writeStatisticsFor(ga, outDir)
        }

      case Failure(ex) =>
        log.error(s"Failed to load GA list file", ex)
    }
  }

  private def readFromFile(filePath: String): Seq[String] = {
    val input = Paths.get(filePath)

    if (input.toFile.exists()) {
      Files.readAllLines(input).asScala.toSeq
    } else {
      throw new IllegalStateException(s"File not found at $input")
    }
  }

  def writeStatisticsFor(ga: String, outDir: Path): Unit = {
    val sanitizedGa = ga.replace(":", "__").replace(".", "_")
    val outFile = Paths.get(outDir.toAbsolutePath.toString, s"$sanitizedGa.csv")

    if(outFile.toFile.exists()){
      log.info(s"Skipping $ga, results already present")
      return
    }

    val libId = dataAccessor.getEntityIdFor(Seq(ga)).get

    var resultCnt = 0

    log.info(s"\t Library ID is $libId")

    val childIds = dataAccessor.awaitGetEntity(libId, Some(1))
      .get
      .getChildren
      .map(p => (p.identifier, p.id))

    log.info(s"\t Got ${childIds.size} programs to process")

    val csvLines = childIds
      .map{ case (v, eid) =>
        if(resultCnt % 10 == 0){
          log.info(s"\t Got $resultCnt results so far")
        }

        resultCnt += 1

        getRunResultsForProgram(ga, v, eid)
      }
      .flatMap {
        case Success(runResult) =>
          Some(runResult)
        case Failure(ex) =>
          log.error("\t Failed to produce run results", ex)
          None
      }
      .map(_.toCSVLine)

    Try(Files.write(outFile, csvLines.toSeq.asJava)) match {
      case Success(_) =>
        log.info(s"Successfully wrote results for library $ga to file ${outFile.toString}")
      case Failure(ex) =>
        log.error(s"Failed to write results for library $ga to file ${outFile.toString}", ex)
    }

  }

  def getRunResultsForProgram(ga: String, v: String, eid: Long): Try[RunResult] = {

    dataAccessor
      .getAnalysisRunsForEntity(eid, Some("TaintFlowSummaryBuilder", "0.0.2"), skip = 0, limit = 10)
      .map(_.find(run => run.state == RunState.Finished))
      .flatMap {
        case Some(run) =>
          val runtime = run.durationMs
          dataAccessor
            .getNoOfFreshAndTotalResults(run.uid)
            .map{ case (freshCnt, totalCnt) =>
              RunResult(eid, ga, v, totalCnt, freshCnt, runtime)
            }
        case None =>
          Failure(new RuntimeException(s"Failed to retrieve run for entity-id $eid, run not present"))
      }
  }


  case class RunResult(eid:Long, ga: String, v: String, totalResults: Int, freshResults: Int, runTime: Long){
    def toCSVLine: String = s"$eid, $ga, $v, $totalResults, $freshResults, $runTime"
  }

}
