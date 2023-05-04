package org.anon.spareuse.eval.studies.evometrics

import org.anon.spareuse.core.model.entities.JavaEntities.JavaClass
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.eval.{encodeString, getAllTypesForProgram, getApiBaseUrl}
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.util.EntityUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}
import spray.json.{JsArray, JsObject, JsString, enrichString}

class EvolutionAnalyzer {

  private val apiBaseUrl = getApiBaseUrl
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def calculateEvolutionMetrics(ga: String, v1: String, v2: String): Try[EvolutionMetricsReport] = {
    val oldGav = ga + ":" + v1
    val newGav = ga + ":" + v2

    val httpClient = HttpClients.createDefault()

    def findPackagesUsed(packageObj: SoftwareEntityData, possibleTargetPackages: Set[String]): Map[String, Seq[Usage]] = {
      val allPackageUsages = packageObj
        .getChildren
        .toSeq
        .flatMap { c =>
          val oldType = c.asInstanceOf[JavaClass]
          getUsages(oldType, httpClient)
            .flatMap { invocation =>
              var longestPackageOpt: Option[String] = None

              possibleTargetPackages.foreach { oldPackageName =>
                if (invocation.targetTypeFqn.startsWith(oldPackageName)) {
                  if (longestPackageOpt.isEmpty || longestPackageOpt.get.length < oldPackageName.length) longestPackageOpt = Some(oldPackageName)
                }
              }

              longestPackageOpt.map( (_, invocation))
            }
        }

      allPackageUsages.groupBy(_._1).mapValues(_.map(_._2))
    }

    val result = Try {
      val allOldTypes = getAllTypesForProgram(oldGav, apiBaseUrl, httpClient).get
      val P_old = allOldTypes.map(_.getParent.get).map(p => (p.name, p)).toMap
      val oldPackageNames = P_old.keySet

      val allNewTypes = getAllTypesForProgram(newGav, apiBaseUrl, httpClient).get
      val P_new = allNewTypes.map(_.getParent.get).map(p => (p.name, p)).toMap
      val newPackageNames = P_new.keySet

      val RELi = P_old
        .mapValues( findPackagesUsed(_, oldPackageNames) )
        .flatMap{ t =>
          t._2.map(t2 => ((t._1, t2._1), t2._2))
        }

      val RELi1 = P_new
        .mapValues( findPackagesUsed(_, newPackageNames) )
        .flatMap { t =>
          t._2.map(t2 => ((t._1, t2._1), t2._2))
        }

      // At this point we have all data necessary for the computations, i.e. Set of packages and classes for both old and
      // new, as well as sets of usage relations between packages.

      // Utility sets introduced in paper
      val PSa = newPackageNames.diff(oldPackageNames)
      val PSr = oldPackageNames.diff(newPackageNames)
      val PSb = oldPackageNames.intersect(newPackageNames)

      def oldSize(s: String): Int = P_old(s).getChildren.size
      def newSize(s: String): Int = P_new(s).getChildren.size

      // External Stability
      val ESr = 1 - (PSr.map(oldSize).sum / allOldTypes.size.toDouble)
      val ESc = 1 - (PSb.map( pName => oldSize(pName) - newSize(pName)).sum / allOldTypes.size.toDouble)
      val ES = ESr * ESc

      // Internal Stability
      def PS_a(Pj: String, Pk: String): Double = 1.0 - (RELi1((Pj, Pk)).count(newUsage => !RELi((Pj, Pk)).contains(newUsage)) / RELi1((Pj, Pk)).size.toDouble)
      def PS_r(Pj: String, Pk: String): Double = 1.0 - (RELi((Pj, Pk)).count(oldUsage => !RELi1((Pj, Pk)).contains(oldUsage)) / RELi((Pj, Pk)).size.toDouble)
      val IS = RELi.keySet.intersect(RELi1.keySet).map(t => (PS_a(t._1, t._2) + PS_r(t._1, t._2)) / 2.0).sum / RELi.keySet.intersect(RELi1.keySet).size.toDouble

      val Stability = (ES + IS) / 2.0


      // External Evolution
      val EE = PSa.map(newSize).sum / (PSb.map(newSize).sum + PSa.map(newSize).sum).toDouble

      // Internal Evolution
      val PN = oldPackageNames.filter{ Pj =>
        newPackageNames.exists(Pk => PSa.contains(Pk) && RELi1.contains((Pj, Pk))) // Find existing packages that interact with newly added packages
      }
      val IE = PN.size / PSb.size.toDouble

      val Evolution = (EE + IE) / 2.0


      EvolutionMetricsReport(ga, v1, v2, IS, ES, Stability, IE, EE, Evolution)
    }

    httpClient.close()

    result
  }



  def getUsages(jc: JavaClass, httpClient: CloseableHttpClient): Seq[Usage] = {
    val getRequest = new HttpGet(apiBaseUrl + "entities/" + encodeString(jc.uid))
    getRequest.setHeader("limit", "1000")
    val response = httpClient.execute(getRequest)

    if (response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve methods for class ${jc.uid} (Status code ${response.getStatusLine.getStatusCode}) ")
    }

    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    val hierarchyUsages = jc.superType.map(Usage(jc.thisType, _)).map( su => Seq(su) ++ jc.interfaceTypes.map(Usage(jc.thisType, _))).getOrElse(Seq.empty[Usage])

    contentT match {
      case Success(JsObject(fields)) =>
        if(fields.contains("Children") && fields("Children").isInstanceOf[JsArray]){
          fields("Children").asInstanceOf[JsArray].elements.collect{
            case jo: JsObject if jo.fields.contains("Children") =>
              jo.fields("Children").asInstanceOf[JsArray].elements.collect{
                case invocationJson: JsObject=>
                  Usage(jc.thisType, invocationJson.fields("TargetType").asInstanceOf[JsString].value)
              }
          }.flatten ++ hierarchyUsages
        } else Seq.empty
      case Failure(ex) =>
        logger.error(s"Failed to retrieve invocation information for class ${jc.uid}", ex)
        throw ex
      case _ =>
        logger.error(s"Invalid result format for class ${jc.uid}")
        throw new IllegalStateException("Invalid result format")

    }
  }


  case class Usage(sourceTypeFqn: String, targetTypeFqn: String)

}
