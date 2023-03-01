package de.tudo.sse.spareuse.eval.performance.cgs
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaClass
import de.tudo.sse.spareuse.eval.performance.{gavToEntityId, getAllTypesForProgram, getAnalysisResultsForEntity, runFinished, triggerAnalysisRun}
import org.apache.http.impl.client.HttpClients
import spray.json.{JsArray, JsNumber, JsObject, JsString}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ReuseBasedCallgraphAnalysis(apiBaseUrl: String) extends WholeProgramCgAnalysis {

  private val httpClient = HttpClients.createDefault()
  private val analysisName = "mvn-partial-callgraphs"
  private val analysisVersion = "1.0.0"

  private var theCg: Option[StitchedCallGraph] = None

  override def prepareData(rootGav: String, dependencyGavs: Set[String]): Try[Unit] = Try {

    theCg = Some(new StitchedCallGraph(rootGav, dependencyGavs))

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
        val types = getAllTypesForProgram(rootGav, apiBaseUrl, httpClient).get
        val cg = PartialCallGraph(rootGav, toMethodLookup(ja))
        theCg.get.addPartialInfo(rootGav, cg, types)
      case _ =>
        throw new IllegalStateException("Expected a JSON Array")
    }

    dependencyGavs
      .foreach{ gav =>
        val entityId = gavToEntityId(gav)
        getAnalysisResultsForEntity(entityId, analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
          case ja: JsArray =>
            val cg = PartialCallGraph(gav, toMethodLookup(ja))
            val types = getAllTypesForProgram(gav, apiBaseUrl, httpClient).get
            theCg.get.addPartialInfo(gav, cg, types)
          case _ =>
            throw new IllegalStateException("Expected a JSON Array")
        }
      }

    logger.info(s"Successfully retrieved ${dependencyGavs.size + 1} partial callgraphs")


  }

  override def buildFullCallgraph(): Try[Any] = Try {
    val cg = theCg.get

    cg.resolveAll()


  }//TODO!


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
    theCg = None
  }


  case class PartialCallGraph(gav: String, methodLookup: Map[Long, MethodRepr])

  case class MethodRepr(id: Long, fqn: String, callSites: List[CallSiteRepr])

  case class CallSiteRepr(pc: Int, isIncomplete: Boolean, targets: List[Long], instrRep: String)

  class StitchedCallGraph(rootGav: String, dependencyGavs: Set[String]){

    private val partialCgs = new mutable.HashMap[String, PartialCallGraph]()

    private val typesLookup = new mutable.HashMap[String, JavaClass]()

    def addPartialInfo(gav: String, partialCallGraph: PartialCallGraph, types: Seq[JavaClass]): Unit = {
      partialCgs.put(gav, partialCallGraph)
      types.foreach{ t => typesLookup.put(t.thisType, t) }
    }

    def resolveAll(): Unit = {
      val reachableRoot = partialCgs(rootGav).methodLookup.size
      val totalMethodCnt = partialCgs.values.map(_.methodLookup.size).sum
      val allCallSitesCnt = partialCgs.values.map(_.methodLookup.values.map(_.callSites.size).sum).sum
      val incompleteCallSitesCnt = partialCgs.values.map(_.methodLookup.values.map(_.callSites.count(_.isIncomplete)).sum).sum
      logger.info(s"Got ${typesLookup.size} types for stitched callgraph")
      logger.info(s"Got ${partialCgs.size} partial CGs  with $totalMethodCnt total methods, $allCallSitesCnt Callsites and $incompleteCallSitesCnt incomplete Callsites for stitched callgraphs")
      logger.info(s"Got $reachableRoot reachable root methods")
      //???
    }




  }
}
