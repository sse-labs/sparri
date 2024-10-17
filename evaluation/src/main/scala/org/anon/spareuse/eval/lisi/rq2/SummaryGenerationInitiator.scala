package org.anon.spareuse.eval.lisi.rq2

import org.anon.spareuse.core.utils
import org.anon.spareuse.eval
import org.apache.http.impl.client.HttpClients
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

object SummaryGenerationInitiator {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    if(args.length != 1){
      log.error("Missing required argument: GA List File")
      log.error("Usage: SummaryGenerationInitiator <ga-list-file>")
      System.exit(-1)
    }

    Try(readFromFile(args(0))) match {
      case Success(gaList) =>
        log.info(s"Found ${gaList.size} GAs at ${args(0)}")

        val client = HttpClients.createDefault()

        val gaReleasesMap = gaList.map{ ga =>
          eval.getAllVersionsForLibrary(ga, eval.getApiBaseUrl, client) match {
            case Success(versions) =>
              log.info(s"\t${versions.size} releases for $ga")
              val sortedReleases = versions.sortWith { (r1, r2) =>
                Try(utils.compareSemanticVersions(r1, r2)) match {
                  case Success(compResult) => compResult < 0
                  case _ => r1.compareTo(r2) < 0
                }
              }
              (ga, sortedReleases)
            case Failure(ex) =>
              log.error(s"Failed to retrieve version list for $ga", ex)
              (ga, Seq.empty)
          }
        }.toMap

        client.close()

        triggerAll(gaReleasesMap)

      case Failure(ex) =>
        log.error(s"Failed to load GA list file", ex)
    }


  }

  private def triggerAll(libReleasesMap: Map[String, Seq[String]]): Unit = {
    var round = 0
    val runIdMap = mutable.Map.empty[String, String]
    val maxRounds = libReleasesMap.values.map(_.size).max

    def nextRound(): Unit ={
      val httpClient = HttpClients.createDefault()

      libReleasesMap
        .filter{ case (_, releases) => releases.size > round }
        .foreach{ case (ga, releases) =>
          val v = releases(round)
          val gav = s"$ga:$v"
          val previousReleaseGavOpt = if(round > 0) Some(s"$ga:${releases(round - 1)}") else None
          val baselineRunOpt = previousReleaseGavOpt.flatMap(runIdMap.get).map(runUrl => runUrl.substring(runUrl.lastIndexOf("/") + 1 ))

          log.info(s"\t - Triggering $gav")

          eval.triggerAnalysisRun(Set(s"$ga!$v"),"TaintFlowSummaryBuilder", "0.0.1", eval.getApiBaseUrl, httpClient, baselineRun = baselineRunOpt) match {
            case Success(runId) =>
              runIdMap.put(gav, runId)
            case Failure(ex) =>
              log.error(s"Failed to trigger run for $gav", ex)
          }

        }

      httpClient.close()
    }

    while(round < maxRounds){
      log.info(s"Starting round #$round of analysis triggers")
      nextRound()
      round += 1
    }


  }

  private def readFromFile(filePath: String): Seq[String] = {
    val input = Paths.get(filePath)

    if(input.toFile.exists()){
      Files.readAllLines(input).asScala.toSeq.slice(2,3)
    } else {
      throw new IllegalStateException(s"File not found at $input")
    }
  }

}
