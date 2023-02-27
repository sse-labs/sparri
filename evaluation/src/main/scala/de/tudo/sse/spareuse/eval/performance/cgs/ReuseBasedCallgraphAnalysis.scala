package de.tudo.sse.spareuse.eval.performance.cgs
import de.tudo.sse.spareuse.eval.performance.{gavToEntityId, getAllTypesForProgram, getAnalysisResultsForEntity, runFinished, triggerAnalysisRun}
import org.apache.http.impl.client.HttpClients
import spray.json.{JsArray, JsNumber, JsObject, JsString}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ReuseBasedCallgraphAnalysis(apiBaseUrl: String) extends WholeProgramCgAnalysis {

  private val httpClient = HttpClients.createDefault()
  private val analysisName = "mvn-partial-callgraphs"
  private val analysisVersion = "1.0.0"

  private var rootCgJson: Option[PartialCallGraph] = None
  private var dependencyCgsJson = mutable.HashSet[PartialCallGraph]()

  override def prepareData(rootGav: String, dependencyGavs: Set[String]): Try[Unit] = Try {

    def toMethodLookup(json: JsArray): Map[Long, MethodRepr] = {
      json.elements.map {
        case jo: JsObject =>
          val mId = jo.fields("mId").asInstanceOf[JsNumber].value.longValue()
          val mFqn = jo.fields("mFqn").asInstanceOf[JsString].value

          val callSites = jo.fields("css").asInstanceOf[JsArray].elements.map {
            case siteObj: JsObject =>
              val pc = siteObj.fields("csPc").asInstanceOf[JsNumber].value.intValue()
              val isIncomplete = siteObj.fields("incomplete").asInstanceOf[JsNumber].value.intValue() > 0
              val instrRep = siteObj.fields("instr").asInstanceOf[JsString].value
              val targets = siteObj.fields("targets").asInstanceOf[JsArray].elements.map {
                case n: JsNumber => n.value.longValue()
                case _ => throw new IllegalStateException("Invalid format of partial results")
              }.toList

              CallSiteRepr(pc, isIncomplete, targets, instrRep)
            case _ =>
              throw new IllegalStateException("Invalid format of partial results")
          }.toList

          (mId, MethodRepr(mId, mFqn, callSites))
        case _ =>
          throw new IllegalStateException("Invalid format of partial results")
      }.toMap
    }



    getAnalysisResultsForEntity(gavToEntityId(rootGav), analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
      case ja: JsArray =>
        val cg = new PartialCallGraph(rootGav, toMethodLookup(ja), null)
        rootCgJson = Some(cg)
      case _ =>
        throw new IllegalStateException("Expected a JSON Array")
    }

    dependencyGavs
      .foreach{ gav =>
        val entityId = gavToEntityId(gav)
        getAnalysisResultsForEntity(entityId, analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
          case ja: JsArray =>
            val cg = new PartialCallGraph(gav, toMethodLookup(ja), null)

            getAllTypesForProgram(gav, apiBaseUrl, httpClient)

            dependencyCgsJson.add(cg)
          case _ =>
            throw new IllegalStateException("Expected a JSON Array")
        }
      }

    logger.info(s"Successfully retrieved ${dependencyGavs.size + 1} partial callgraphs")


  }

  override def buildFullCallgraph(): Try[Any] = Try{null}//TODO!


  def ensureAllPartialResultsPresent(allGavs: Set[String]): Try[Unit] = Try {
    triggerAnalysisRun(allGavs.map(gavToEntityId), analysisName, analysisVersion, apiBaseUrl, httpClient) match {
      case Success(runLocation) =>
        logger.info(s"Successfully triggered analysis run. Waiting for run at $runLocation to complete...")

        while(!runFinished(runLocation, apiBaseUrl, httpClient).get){
          Thread.sleep(1000)
          logger.debug(s"Waiting for run to finish: $runLocation ...")
        }

        logger.info("All partial callgraphs are available.")
      case Failure(ex) =>
        logger.error("Failed to trigger analysis run for partial callgraphs", ex)
    }
  }

  def close(): Unit = httpClient.close()

  def cleanup(): Unit = {
    rootCgJson = None
    dependencyCgsJson = new mutable.HashSet[PartialCallGraph]()
  }


  class PartialCallGraph(gav: String, methodLookup: Map[Long, MethodRepr], typeHierarchy: Any)

  case class MethodRepr(id: Long, fqn: String, callSites: List[CallSiteRepr])

  case class CallSiteRepr(pc: Int, isIncomplete: Boolean, targets: List[Long], instrRep: String)
}
