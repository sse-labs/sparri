package org.tud.cgcrawling.storage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.slf4j.{Logger, LoggerFactory}
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.model.LibraryCallGraphEvolution


import scala.concurrent.Await
import scala.language.postfixOps

class StoragePerformanceTest extends AnyFlatSpec with must.Matchers {

  import org.tud.cgcrawling.storage.StorageHandlerType._

  private val largeSizeJarLibrary: (String, String) = ("org.codehaus.plexus", "plexus-utils")
  private val midSizeJarLibrary: (String, String) = ("com.joestelmach", "natty")
  private val smallSizeJarLibrary: (String, String) = ("org.toucanpdf", "toucanpdf")

  private val config: Configuration = new Configuration()
  private val log: Logger = LoggerFactory.getLogger(this.getClass)


  "A StorageHandler" must "be faster than the other" in {
    compareStorageTimesForLibrary(smallSizeJarLibrary)
  }

  private def compareStorageTimesForLibrary(library: (String, String)): Unit = {

    buildEvolutionFor(library._1, library._2, config)(log) match {
      case Some(evo) =>
        log.info("Starting storage run for Neo4j ...")
        val neo4jDbStats = measureStorageTime(evo, Neo4jOnly)
        log.info(s"Finished storage run for Neo4j: ${neo4jDbStats._1} s Startup, ${neo4jDbStats._2} s Storage")
        log.info("Starting storage run for Hybrid ...")
        val hybridDbStats = measureStorageTime(evo, Hybrid)
        log.info(s"Finished storage run for Hybrid: ${hybridDbStats._1} s Startup, ${hybridDbStats._2} s Storage")

        log.info(s"Performance Report for ${evo.libraryName} with ${evo.methodEvolutions().size} methods and ${evo.methodInvocationEvolutions().size} inovcations")
        log.info(s"Neo4j : ${neo4jDbStats._1}s / ${neo4jDbStats._2}s")
        log.info(s"Hybrid: ${hybridDbStats._1}s / ${hybridDbStats._2}s")

        assert(neo4jDbStats._2 > hybridDbStats._2)
      case None =>
        log.error("Failed to generate data for performance tests.")
    }

    log.info("Shutting down system...")
  }

  private def measureStorageTime(evo: LibraryCallGraphEvolution, handlerType: StorageHandlerType): (Long, Long) = {
    val startTime: Long = System.currentTimeMillis()

    val storageHandler: StorageHandler = handlerType match {
      case Neo4jOnly => new GraphDbStorageHandler(config)
      case Hybrid => new HybridElasticAndGraphDbStorageHandler(config)
    }

    val startupCompleteTime: Long = System.currentTimeMillis()

    storageHandler.storeCallGraphEvolution(evo) match {
      case GraphDbStorageResult(_, false) =>
        log.error("Failure while storing")
      case _ =>
    }

    val storageCompleteTime: Long = System.currentTimeMillis()

    val startupTime = startupCompleteTime - startTime
    val storageTime = storageCompleteTime - startupCompleteTime

    (startupTime / 1000L, storageTime / 1000L)
  }


}

object StorageHandlerType extends Enumeration {
  type StorageHandlerType = Value

  val Neo4jOnly, Hybrid = Value
}
