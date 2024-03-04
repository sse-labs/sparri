package org.anon.spareuse.client

import org.anon.spareuse.client.analyses.IFDSTaintFlowAnalysis

import java.io.File

object ClientAnalysisApplication {

  def main(args: Array[String]): Unit = {
    if(args.length < 2) throw new IllegalArgumentException(s"Usage: ClientAnalysisApplication <classes-dir> <pom-file>")

    val theAnalysis = new IFDSTaintFlowAnalysis(new File(args(0)), new File(args(1)))

    if(theAnalysis.checkRequirements()){
      println("Analysis requirements are met.")
      theAnalysis.execute()
    } else {
      println("Analysis requirements not satisfied")
    }
  }

}
