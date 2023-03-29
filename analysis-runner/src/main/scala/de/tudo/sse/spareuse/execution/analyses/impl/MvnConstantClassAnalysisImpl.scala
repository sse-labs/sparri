package de.tudo.sse.spareuse.execution.analyses.impl

import de.tudo.sse.spareuse.core.formats
import de.tudo.sse.spareuse.core.formats.{MapResultFormat, NamedPropertyFormat, NumberFormat, ObjectResultFormat}
import de.tudo.sse.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaLibrary}
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData
import de.tudo.sse.spareuse.core.utils.toHex
import de.tudo.sse.spareuse.execution.analyses.{AnalysisImplementation, Result}

import scala.collection.mutable
import scala.util.Try

class MvnConstantClassAnalysisImpl extends AnalysisImplementation {

  private val resultFormat = MapResultFormat(formats.StringFormat,
    ObjectResultFormat(Set(NamedPropertyFormat("noOfOccurrences", NumberFormat), NamedPropertyFormat("noOfUniqueOccurrences", NumberFormat))))

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis("mvn-constant-classes", "1.0.0", "TBD", "built-in",
    Set("java", "scala"),  resultFormat, SoftwareEntityKind.Library)

  override val inputBatchProcessing: Boolean = true

  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Class

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {
    if (inputs.exists(e => !e.isInstanceOf[JavaLibrary])) {
      log.warn(s"Execution of analysis $name:$version not possible: Inputs must be of kind 'Library'")
      false
    } else {
      true
    }
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[Result]] = Try {

    if(!rawConfig.isBlank)
      log.warn("Non-Empty configuration will be ignored, this analysis does not support configuration options")

    inputs.map { case library: JavaLibrary =>

      log.debug(s"Starting to process library ${library.identifier} ...")

      val releasesCnt = library.getChildren.size

      val allClasses = library.getChildren.toList.flatMap(program => program.getChildren).
        flatMap(packageObj => packageObj.getChildren).map{
        case jc: JavaClass => jc
        case _ => throw new IllegalArgumentException("Corrupt input entity hierarchy")
      }

      val classToHashesMap = allClasses.map { jc => (jc.identifier, mutable.ListBuffer.empty[String]) }.toMap

      allClasses.foreach(jc => classToHashesMap(jc.identifier).append(jc.binaryHash.map(toHex).getOrElse{
        log.warn(s"Class without hash: ${jc.identifier}")
        ""
      }))

      val resultMap = classToHashesMap
        .mapValues(b => ClassCountResult(b.size, b.distinct.size))

      log.debug(s"Results for library [${library.identifier}]:")
      log.debug(s"-- #Releases: $releasesCnt")
      classToHashesMap.mapValues(b => (b.size, b.distinct.size)).foreach{t => log.debug(s"-- ${t._1} -> ${t._2._2}/${t._2._1}")}


      Result(resultMap, Set(library))
    }.toSet
  }

  case class ClassCountResult(noOfOccurrences: Int, noOfUniqueOccurrences: Int)
}
