package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.formats
import org.anon.spareuse.core.formats.{MapResultFormat, NamedPropertyFormat, NumberFormat, ObjectResultFormat}
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaLibrary}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.utils.toHex
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisImplementationDescriptor, AnalysisResult, FreshResult}

import scala.collection.mutable
import scala.util.Try

class MvnConstantClassAnalysisImpl extends AnalysisImplementation {

  override val descriptor: AnalysisImplementationDescriptor = MvnConstantClassAnalysisImpl

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {
    if (inputs.exists(e => !e.isInstanceOf[JavaLibrary])) {
      log.warn(s"Execution of analysis ${descriptor.fullName} not possible: Inputs must be of kind 'Library'")
      false
    } else {
      true
    }
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = Try {

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

      val classToHashesMap = allClasses.map { jc => (jc.thisType, mutable.ListBuffer.empty[String]) }.toMap

      allClasses.foreach(jc => classToHashesMap(jc.thisType).append(jc.binaryHash.map(toHex).getOrElse{
        log.warn(s"Class without hash: ${jc.name}")
        ""
      }))

      val resultMap = classToHashesMap
        .view
        .mapValues(b => ClassCountResult(b.size, b.distinct.size))
        .toMap

      log.debug(s"Results for library [${library.identifier}]:")
      log.debug(s"-- #Releases: $releasesCnt")
      classToHashesMap
        .view
        .mapValues(b => (b.size, b.distinct.size))
        .toMap.foreach{t => log.debug(s"-- ${t._1} -> ${t._2._2}/${t._2._1}")}


      FreshResult(resultMap, Set(library))
    }.toSet
  }

  case class ClassCountResult(noOfOccurrences: Int, noOfUniqueOccurrences: Int)
}

object MvnConstantClassAnalysisImpl extends AnalysisImplementationDescriptor {

  private val resultFormat = MapResultFormat(
    formats.StringFormat,
    ObjectResultFormat(Set(
      NamedPropertyFormat("noOfOccurrences", NumberFormat, "Number of library releases this class was present in."),
      NamedPropertyFormat("noOfUniqueOccurrences", NumberFormat, "Number of unique versions this class had in its history.")
    )),
    keyExplanation = "Fully qualified class name for every class in the history of this library",
    valueExplanation = "Constancy information on this class"
  )

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis(
    "mvn-constant-classes",
    "1.0.0",
    "Analysis that processes a Maven library (GA-Tuple) and computes the number of times each class appears in a release, as well as the number of unique versions of every class. MD5 is used to derive class equality.",
    "Scala, built-in facilities",
    Set("java", "scala"),
    resultFormat,
    SoftwareEntityKind.Library,
    doesBatchProcessing = true,
    isIncremental = false
  )

  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Class


}
