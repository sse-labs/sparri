package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.AnalysisData
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaMethod, JavaProgram}
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.utils.EnhancedLogging
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.DefinedMethod
import org.anon.spareuse.execution.analyses.impl.cg.DefaultRTACallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.MethodIFDSRep
import spray.json.enrichString

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class DefaultIFDSSuperGraphStitcher(dataAccessor: DataAccessor,
                                    inputs: Set[JavaProgram],
                                    analysisToStitch: AnalysisData,
                                    jreVersionToLoad: Option[String]) extends DefaultIFDSMethodRepJsonFormat with EnhancedLogging{

  private[ifds] val cgBuilder = new DefaultRTACallGraphBuilder(inputs, jreVersionToLoad)
  private[ifds] val superGraph = new StitchedGraph()

  private[ifds] var missingSummariesCount = 0

  private[ifds] def getPartialGraphFor(dm: DefinedMethod): Try[IFDSMethodGraph] = Try {
    val theResults = dataAccessor.getJSONResultsFor(???, analysisFilter = Some(analysisToStitch.name, analysisToStitch.version), limit = 1, skip = 0).get

    if(theResults.isEmpty)
      throw new IllegalStateException(s"Precondition not met: No results for analysis ${analysisToStitch.name}:${analysisToStitch.version} on method ${dm.toString}")

    val graphJson: String = theResults.head.content.asInstanceOf[String]

    IFDSMethodGraph(graphJson.parseJson.convertTo[MethodIFDSRep])
  }



  def stitchFrom(entryPoint: JavaMethod): Try[Unit] = {

    log.info(s"Stitching IFDS graph for entrypoint: ${entryPoint.toString}")

    // Update the current CallGraphView with the latest entrypoint
    timedOp(s"Update underlying callgraph for entrypoint ${entryPoint.toString}", () => cgBuilder.buildFrom(entryPoint)) match {
      case Success(_) =>
        stitchAll(cgBuilder.asDefinedMethod(entryPoint))
      case Failure(ex) =>
        log.error(s"Failed to update callgraph for entrypoint ${entryPoint.name}", ex)
        Failure(ex)
    }
  }

  private[ifds] def stitchAll(entryPoint: DefinedMethod): Try[Unit] = Try {
   superGraph.getMethodGraphFor(entryPoint) match {
      case Some(partialGraph) =>

        val callResolutions = cgBuilder.getGraph.calleesOf(entryPoint).toMap

        partialGraph
          .statementNodes
          .filter(_.isCallNode)
          .foreach{ callStmt =>
            if(!callResolutions.contains(callStmt.stmtPc))
              log.warn(s"Unknown callsite: No callee information for PC=${callStmt.stmtPc} (Known callsites: ${callResolutions.keys.mkString(",")}")

            callResolutions(callStmt.stmtPc)
              .foreach{ targetMethod =>
                stitch(partialGraph, callStmt, targetMethod)
              }
          }

      case None =>
        missingSummariesCount += 1
    }
  }

  private[ifds] def stitch(currentGraph: IFDSMethodGraph, callStmt: StatementNode, target: DefinedMethod): Unit = {

    superGraph.getMethodGraphFor(target) match {
      case Some(targetGraph) =>
      case None =>
        log.error(s"Missing summary for call target: ${target.toString}")
    }

  }


  class StitchedGraph {

    private[ifds] val partialGraphs: mutable.Map[DefinedMethod, IFDSMethodGraph] = mutable.HashMap()


    def getMethodGraphFor(dm: DefinedMethod): Option[IFDSMethodGraph] = {
      if(!partialGraphs.contains(dm)){

        timedOp(s"Retrieve IFDS summary for method ${dm.toString}", () => getPartialGraphFor(dm)) match {
          case Success(partialGraph) =>
            partialGraphs(dm) = partialGraph

          case Failure(ex) =>
            log.error(s"Failed to retrieve partial result", ex)
        }
      }

      partialGraphs.get(dm)
    }

  }


}
