package org.anon.spareuse.core

import scala.util.Try

package object utils {
  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString

  def fromHex(hexString: String): Array[Byte] = BigInt(hexString, 16).toByteArray

  def tryGetEnvOrElse(envName: String, elseProducer: () => String): String = {
    val envValue = System.getenv(envName)

    if (envValue == null) elseProducer.apply()
    else envValue
  }

  private final val regex = """(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:-([\w\d]+(?:\.[\w\d]+)*))?(?:\+([\w\d]+(?:\.[\w\d]+)*))?""".r

  case class SemVer(major: Int, minor: Int, patch: Int, preRelease: Option[String]){

    def getUpdateType(toCompare: SemVer): String= {
      if(major != toCompare.major) "MAJOR"
      else if(minor != toCompare.minor) "MINOR"
      else if(patch != toCompare.patch) "PATCH"
      else if(preRelease != toCompare.preRelease) "PRERELEASE"
      else "NONE"
    }
  }


  def parseSemVer(version: String): Try[SemVer] = Try {
    version match {
      case regex(major, minor, patch, preRelease, _) =>
        SemVer(major.toInt, Option(minor).getOrElse("0").toInt, Option(patch).getOrElse("0").toInt, Option(preRelease))
      case _ => throw new IllegalArgumentException("Invalid version format")
    }
  }

  def compareSemanticVersions(version1: String, version2: String): Int = {

    val ver1 = parseSemVer(version1).get
    val ver2 = parseSemVer(version2).get

    if (ver1.major != ver2.major) {
      ver1.major.compareTo(ver2.major)
    } else if (ver1.minor != ver2.minor) {
      ver1.minor.compareTo(ver2.minor)
    } else if (ver1.patch != ver2.patch) {
      ver1.patch.compareTo(ver2.patch)
    } else {
      // Compare pre-release versions if present
      val preReleaseComparison = (ver1.preRelease, ver2.preRelease) match {
        case (Some(pre1), Some(pre2)) => pre1.compareTo(pre2)
        case (Some(_), None) => -1
        case (None, Some(_)) => 1
        case (None, None) => 0
      }

      preReleaseComparison
    }
  }

  case class TimedResult [T] (result: T, timeMs: Long)

  def wcTime[T](implicit op:() => T): TimedResult[T] = {
    val start = System.currentTimeMillis()
    val result = op.apply()
    val duration = System.currentTimeMillis() - start
    TimedResult(result, duration)
  }


}
