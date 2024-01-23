package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.AnalysisData
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaMethod, JavaProgram}
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.utils.EnhancedLogging
import org.anon.spareuse.execution.analyses.impl.cg.DefaultRTACallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.{MethodIFDSRep, StatementRep}
import spray.json.enrichString

import scala.util.{Failure, Success, Try}

class IFDSSuperGraphStitcher(dataAccessor: DataAccessor,
                             inputs: Set[JavaProgram],
                             analysisToStitch: AnalysisData,
                             jreVersionToLoad: Option[String]) extends DefaultIFDSMethodRepJsonFormat with EnhancedLogging{

  val cgBuilder = new DefaultRTACallGraphBuilder(inputs, jreVersionToLoad)

  private[ifds] var missingSummariesCount = 0

  private[ifds] def getPartialGraphFor(dm: cgBuilder.DefinedMethod): Try[MethodIFDSRep] = Try {
    val theResults = dataAccessor.getJSONResultsFor(dm.javaMethod.uid, analysisFilter = Some(analysisToStitch.name, analysisToStitch.version), limit = 1, skip = 0).get

    if(theResults.isEmpty)
      throw new IllegalStateException(s"Precondition not met: No results for analysis ${analysisToStitch.name}:${analysisToStitch.version} on method ${dm.toString}")

    val graphJson: String = theResults.head.content.asInstanceOf[String]

    graphJson.parseJson.convertTo[MethodIFDSRep]
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

  def stitchAll(entryPoint: cgBuilder.DefinedMethod): Try[Unit] = Try {
    timedOp(s"Retrieve IFDS summary for method ${entryPoint.toString}", () => getPartialGraphFor(entryPoint)) match {
      case Success(partialGraph) =>

        val callResolutions = cgBuilder.getGraph.calleesOf(entryPoint).toMap

        partialGraph
          .statements
          .filter(_.calleeMethodName.nonEmpty)
          .foreach{ callStmt =>
            if(!callResolutions.contains(callStmt.pc))
              log.warn(s"Unknown callsite: No callee information for PC=${callStmt.pc} (Known callsites: ${callResolutions.keys.mkString(",")}")

            callResolutions(callStmt.pc)
              .foreach{ targetMethod =>
                stitch(partialGraph, callStmt, targetMethod)
              }
          }

      case Failure(ex) =>
        missingSummariesCount += 1
        log.error(s"Failed to retrieve partial result", ex)
        throw ex
    }
  }

  def stitch(currentGraph: MethodIFDSRep, callStmt: StatementRep, target: cgBuilder.DefinedMethod): Unit = {
    ???
  }


}
