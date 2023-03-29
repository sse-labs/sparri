package de.tudo.sse.spareuse.eval.studies.apicompat

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.eval
import de.tudo.sse.spareuse.eval.getAllMethodsForClass
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

class BreakingChangeAnalysis {

  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val apiBaseUrl = "http://ls5vs029.cs.tu-dortmund.de:9090/api/"

  def calculateChanges(oldReleaseGav: String, newReleaseGav: String): Try[BreakingChangeReport] = Try {
    implicit val httpClient: CloseableHttpClient = HttpClients.createDefault()

    val allOldTypes = getAllTypeData(oldReleaseGav).get
    val oldTypesLookup = allOldTypes.map(jc => (jc.thisType, jc)).toMap
    val allNewTypes = getAllTypeData(newReleaseGav).get
    val newTypesLookup = allNewTypes.map(jc => (jc.thisType, jc)).toMap

    val oldMethodLookup = allOldTypes
      .flatMap{ jc =>
        jc
          .getChildren
          .map(_.asInstanceOf[JavaMethod])
          .map(jm => (MethodIdentifier(jc.thisType, jm.name, jm.paramTypes), jm))
      }
      .toMap

    val newMethodLookup = allNewTypes
      .flatMap { jc =>
        jc
          .getChildren
          .map(_.asInstanceOf[JavaMethod])
          .map(jm => (MethodIdentifier(jc.thisType, jm.name, jm.paramTypes), jm))
      }
      .toMap

    val classesRemoved = allOldTypes.filterNot(_.isInterface).toSet.diff(allNewTypes.filterNot(_.isInterface).toSet).size
    val interfacesRemoved = allOldTypes.filter(_.isInterface).toSet.diff(allNewTypes.filter(_.isInterface).toSet).size
    val methodsRemoved = oldMethodLookup.keys.toSet.diff(newMethodLookup.keys.toSet).size //TODO: Parameter types changed?

    val returnTypesChanged = oldMethodLookup.count{ t =>
      val oldIdent = t._1
      val oldMethod = t._2

      newMethodLookup.contains(oldIdent) && !newMethodLookup(oldIdent).returnType.equals(oldMethod.returnType)
    }

    // Count all interface types, for which new methods exist in the second release that have not been present in the first release
    val methodsAddedToInterface = allOldTypes
      .filter(_.isInterface)
      .count{ i =>
        val allOldMethodIdents = i.getChildren.map(e => MethodIdentifier(i.thisType, e.asInstanceOf[JavaMethod]))

        if(!newTypesLookup.contains(i.thisType) || !newTypesLookup(i.thisType).isInterface)  false
        else {
          val allNewMethodIdents = newTypesLookup(i.thisType).getChildren.map(e => MethodIdentifier(i.thisType, e.asInstanceOf[JavaMethod]))
          allNewMethodIdents.diff(allOldMethodIdents).nonEmpty
        }
      }

    //TODO: Calculate all values

    BreakingChangeReport(
      methodsRemoved,
      classesRemoved,
      paramTypesChanged = ???,
      returnTypesChanged,
      interfacesRemoved,
      numberOfArgsChanged = ???,
      methodsAddedToInterface,
      removedFromSuperclasses = ???,
      methodAccessibilityDecreased = ???,
      methodNowFinal = ???,
      abstractMethodAdded = ???,
      addedFinalModifier = ???,
      addedAbstractModifier = ???
    )
  }

  private def getAllTypeData(gav: String)(implicit httpClient: CloseableHttpClient): Try[Seq[JavaClass]] = Try {
    eval
      .getAllTypesForProgram(gav, apiBaseUrl, httpClient)
      .get
      .map { jc =>
        val methods: Set[SoftwareEntityData] = getAllMethodsForClass(jc.uid, apiBaseUrl, httpClient).get.toSet
        jc.setChildren(methods)
        jc
      }
  }

}
