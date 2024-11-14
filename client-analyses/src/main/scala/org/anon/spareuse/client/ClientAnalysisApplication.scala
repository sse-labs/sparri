package org.anon.spareuse.client

import org.anon.spareuse.client.analyses.IFDSTaintFlowAnalysis
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import java.nio.file.Paths
import scala.util.{Failure, Success}

object ClientAnalysisApplication {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    if(args.length < 1 || args.length > 2) throw new IllegalArgumentException(s"Usage: ClientAnalysisApplication <classes-dir> <pom-file> OR ClientAnalysisApplication <maven-project-root>")

    val classesDir = if(args.length == 1) Paths.get(args(0), "target", "classes").toFile else new File(args(0))
    val pomFile = if(args.length == 1) Paths.get(args(0), "pom.xml").toFile else new File(args(1))

    val theAnalysis = new IFDSTaintFlowAnalysis(classesDir, pomFile)

    if(theAnalysis.checkRequirements()){
      log.info("Analysis requirements are met.")
      theAnalysis.initialize()
      theAnalysis.execute() match {
        case Success(_) =>
          log.info(s"Successfully finished analysis execution")
        case Failure(ex) =>
          log.error(s"Error while analyzing project: ${ex.getMessage}", ex)
      }
      theAnalysis.close()
    } else {
      log.error("Analysis requirements not satisfied")
      theAnalysis.close()
    }
  }

}
