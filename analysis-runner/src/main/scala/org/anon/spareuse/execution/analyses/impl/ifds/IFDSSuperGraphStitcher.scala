package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaMethod, JavaProgram}
import org.anon.spareuse.core.storage.DataAccessor
import org.anon.spareuse.core.utils.EnhancedLogging
import org.anon.spareuse.execution.analyses.impl.cg.DefaultRTACallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.MethodIFDSRep
import org.opalj.br.DefinedMethod
import org.slf4j.{Logger, LoggerFactory}
import spray.json.enrichString

import scala.util.{Failure, Success, Try}

class IFDSSuperGraphStitcher(dataAccessor: DataAccessor,
                             inputs: Set[JavaProgram],
                             jreVersionToLoad: Option[String]) extends DefaultIFDSMethodRepJsonFormat with EnhancedLogging{

  val cgBuilder = new DefaultRTACallGraphBuilder(inputs, jreVersionToLoad)

  private[ifds] def getPartialGraphFor(dm: DefinedMethod): Try[MethodIFDSRep] = Try {
    val graphJson: String = ???

    graphJson.parseJson.convertTo[MethodIFDSRep]
  }



  def stitchFrom(entryPoint: JavaMethod): Try[Unit] = {

    log.info(s"Stitching IFDS graph for entrypoint: ${entryPoint.toString}")

    // Update the current CallGraphView with the latest entrypoint
    timedOp(s"Update underlying callgraph for entrypoint ${entryPoint.toString}", () => cgBuilder.buildFrom(entryPoint) match {
      case Success(callGraph) =>
        var currentMethod = cgBuilder.asDefinedMethod(entryPoint)



        ???
      case Failure(ex) =>
        log.error(s"Failed to update callgraph for entrypoint ${entryPoint.name}", ex)
        Failure(ex)
    }
  }


}
