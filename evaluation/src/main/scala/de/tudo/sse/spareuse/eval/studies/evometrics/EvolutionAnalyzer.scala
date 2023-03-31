package de.tudo.sse.spareuse.eval.studies.evometrics

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaClass
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.eval.{encodeString, getAllTypesForProgram}
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.util.EntityUtils
import org.slf4j.{Logger, LoggerFactory}

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}
import spray.json.{JsArray, JsObject, JsString, enrichString}

class EvolutionAnalyzer {

  private val apiBaseUrl = "http://ls5vs029.cs.tu-dortmund.de:9090/api/"
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def calculateEvolutionMetrics(ga: String, v1: String, v2: String): Try[EvolutionMetricsReport] = {
    val oldGav = ga + ":" + v1
    val newGav = ga + ":" + v2

    val httpClient = HttpClients.createDefault()

    def findPackagesUsed(packageObj: SoftwareEntityData, possibleTargetPackages: Set[String]): Map[String, Int] = {
      val allPackageUsages = packageObj.getChildren.toSeq.flatMap { c =>
        val oldType = c.asInstanceOf[JavaClass]
        getInvokedTypeNamesFor(oldType, httpClient).flatMap { invokedType =>
          var longestPackageOpt: Option[String] = None

          possibleTargetPackages.foreach { oldPackageName =>
            if (invokedType.startsWith(oldPackageName)) {
              if (longestPackageOpt.isEmpty || longestPackageOpt.get.length < oldPackageName) longestPackageOpt = Some(oldPackageName)
            }
          }

          longestPackageOpt
        }
      }

      allPackageUsages.groupBy(identity).mapValues(_.size)
    }

    Try {
      val allOldTypes = getAllTypesForProgram(oldGav, apiBaseUrl, httpClient).get
      val P_old = allOldTypes.map(_.getParent.get).map(p => (p.name, p)).toMap
      val oldPackageNames = P_old.keySet

      val allNewTypes = getAllTypesForProgram(newGav, apiBaseUrl, httpClient).get
      val P_new = allNewTypes.map(_.getParent.get).map(p => (p.name, p)).toMap
      val newPackageNames = P_new.keySet

      val oldPackageUsageRelation = P_old
        .mapValues( findPackagesUsed(_, oldPackageNames) )

      val newPackageUsageRelation = P_new
        .mapValues( findPackagesUsed(_, newPackageNames) )

    }

    httpClient.close()


    ???
  }



  def getInvokedTypeNamesFor(jc: JavaClass, httpClient: CloseableHttpClient): Seq[String] = {
    val getRequest = new HttpGet(apiBaseUrl + "entities/" + encodeString(jc.uid))
    getRequest.setHeader("limit", "1000")
    val response = httpClient.execute(getRequest)

    if (response.getStatusLine.getStatusCode != 200) {
      response.close()
      throw new IllegalStateException(s"Failed to retrieve methods for class ${jc.uid} (Status code ${response.getStatusLine.getStatusCode}) ")
    }

    val contentT = Try(EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8).parseJson)
    response.close()

    contentT match {
      case Success(JsObject(fields)) =>
        if(fields.contains("Children") && fields("Children").isInstanceOf[JsArray]){
          fields("Children").asInstanceOf[JsArray].elements.collect{
            case jo: JsObject if jo.fields.contains("Children") =>
              jo.fields("Children").asInstanceOf[JsArray].elements.collect{
                case invocationJson: JsObject=>
                  invocationJson.fields("TargetType").asInstanceOf[JsString].value
              }
          }.flatten
        } else Seq.empty
      case Failure(ex) =>
        logger.error(s"Failed to retrieve invocation information for class ${jc.uid}", ex)
        throw ex

    }
  }

}
