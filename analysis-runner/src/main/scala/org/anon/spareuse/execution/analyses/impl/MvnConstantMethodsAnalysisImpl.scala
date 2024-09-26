package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.formats
import org.anon.spareuse.core.formats.{ListResultFormat, NamedPropertyFormat, ObjectResultFormat}
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.JavaLibrary
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.utils.{SemVer, compareSemanticVersions, parseSemVer}
import org.anon.spareuse.execution.analyses.impl.MvnConstantMethodsAnalysisImpl.{LibraryResult, MethodIdent, ReleaseResult}
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisImplementationDescriptor, AnalysisResult, FreshResult}

import scala.collection.mutable
import scala.util.{Success, Try}

class MvnConstantMethodsAnalysisImpl extends AnalysisImplementation {
  override val descriptor: AnalysisImplementationDescriptor = MvnConstantClassAnalysisImpl

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {
    if (inputs.exists(e => !e.isInstanceOf[JavaLibrary])) {
      log.warn(s"Execution of analysis ${descriptor.fullName} not possible: Inputs must be of kind 'Library'")
      false
    } else {
      true
    }
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = {
    if (!rawConfig.isBlank)
      log.warn("Non-Empty configuration will be ignored, this analysis does not support configuration options")

    Try { inputs.map(input => buildResult(input.asInstanceOf[JavaLibrary]).get).toSet }
  }

  private def buildResult(lib: JavaLibrary): Try[AnalysisResult] = Try {
    val releases = lib.getPrograms

    val numReleases = releases.size
    val avgMethodsPerRelease = releases.map(_.allMethods.size).sum.toDouble / numReleases

    val totalMethodVersionsSeen = mutable.Map.empty[MethodIdent, mutable.Set[Int]]

    val sortedReleases = releases.toSeq.sortWith{ (r1, r2) =>
      Try(compareSemanticVersions(r1.v, r2.v)) match {
        case Success(compResult) => compResult < 0
        case _ => r1.v.compareTo(r2.v) < 0
      }
    }

    var lastReleaseMethods: Option[Map[MethodIdent, Int]] = None
    var lastReleaseVersion: Option[SemVer] = None

    val releaseInfo = sortedReleases.map { currentRelease =>
      val currentReleaseMethods = currentRelease
        .allMethods
        .map(jm => MethodIdent(jm.enclosingClass.get.thisType, jm.name, jm.descriptor) -> jm.methodHash)
        .toMap

      val totalMethods = currentReleaseMethods.size

      var overallNewMethodsSeen = 0
      var updateNewMethodsSeen = 0

      currentRelease
        .allMethods
        .foreach{ currMethod =>
          val currIdent = MethodIdent(currMethod.enclosingClass.get.thisType, currMethod.name, currMethod.descriptor)

          if(!totalMethodVersionsSeen.contains(currIdent)){
            overallNewMethodsSeen += 1
            totalMethodVersionsSeen.put(currIdent, mutable.Set(currMethod.methodHash))
          } else if(!totalMethodVersionsSeen(currIdent).contains(currMethod.methodHash)){
            overallNewMethodsSeen += 1
            totalMethodVersionsSeen(currIdent).add(currMethod.methodHash)
          }

          if(lastReleaseMethods.isDefined){
            val last = lastReleaseMethods.get
            if(!last.contains(currIdent) || last(currIdent) != currMethod.methodHash) updateNewMethodsSeen += 1
          } else {
            updateNewMethodsSeen += 1
          }

        }

      lastReleaseMethods = Some(currentReleaseMethods)

      val currentVersion = parseSemVer(currentRelease.v).toOption
      val updateType = if(currentVersion.isDefined && lastReleaseVersion.isDefined)
        currentVersion.get.getUpdateType(lastReleaseVersion.get) else "NO_SEM_VER"

      lastReleaseVersion = currentVersion

      ReleaseResult(currentRelease.v, totalMethods, overallNewMethodsSeen, updateNewMethodsSeen, updateType)
    }.toList

    val theResult = LibraryResult(lib.libraryName, numReleases, avgMethodsPerRelease, releaseInfo)

    FreshResult(theResult, Set(lib))
  }

}

object MvnConstantMethodsAnalysisImpl extends AnalysisImplementationDescriptor {

  private val releaseInfoFormat = ObjectResultFormat(
    NamedPropertyFormat("version", formats.StringFormat),
    NamedPropertyFormat("totalMethods", formats.NumberFormat),
    NamedPropertyFormat("overallNewMethods", formats.NumberFormat),
    NamedPropertyFormat("updateNewMethods", formats.NumberFormat),
    NamedPropertyFormat("updateType", formats.StringFormat)
  )

  private val resultFormat = ObjectResultFormat(
    NamedPropertyFormat("ga", formats.StringFormat, "GA of this library"),
    NamedPropertyFormat("numReleases", formats.NumberFormat, "Number of library releases"),
    NamedPropertyFormat("avgMethodsPerRelease", formats.NumberFormat, "Number of average methods per release"),
    NamedPropertyFormat("releaseInfo", ListResultFormat(releaseInfoFormat), "Statistics on each release")
  )

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis(
    name = "mvn-constant-methods",
    version = "1.0.0",
    description = "Processes Maven library (GA-Tuple) and computes basic statistics on how often methods actually change on updates",
    builtOn = "built-in facilities",
    Set("java", "scala"),
    resultFormat,
    SoftwareEntityKind.Library,
    doesBatchProcessing = true,
    isIncremental = false
  )


  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Method

  private case class MethodIdent(classFqn: String, methodName: String, descriptor: String) {
    override def toString: String = s"$classFqn.$methodName : $descriptor"

    override def hashCode(): Int = classFqn.hashCode + 11*methodName.hashCode + 17*descriptor.hashCode
  }

  case class LibraryResult(ga: String, numReleases: Int, avgMethodsPerRelease: Double, releaseInfo: List[ReleaseResult])
  case class ReleaseResult(version: String, totalMethods: Int, overallNewMethods: Int, updateNewMethods: Int, updateType: String)
}
