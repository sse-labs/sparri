package org.anon.spareuse.client

import org.anon.spareuse.client.analyses.IFDSTaintFlowAnalysis
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import scala.util.{Failure, Success}

object ClientAnalysisApplication {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    if(args.length < 2) throw new IllegalArgumentException(s"Usage: ClientAnalysisApplication <classes-dir> <pom-file>")

    val theAnalysis = new IFDSTaintFlowAnalysis(new File(args(0)), new File(args(1)))

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
