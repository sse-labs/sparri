package org.anon.spareuse.core

package object utils {
  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString

  def fromHex(hexString: String): Array[Byte] = BigInt(hexString, 16).toByteArray

  def tryGetEnvOrElse(envName: String, elseProducer: () => String): String = {
    val envValue = System.getenv(envName)

    if (envValue == null) elseProducer.apply()
    else envValue
  }

  def compareSemanticVersions(version1: String, version2: String): Int = {
    val regex = """(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:-([\w\d]+(?:\.[\w\d]+)*))?(?:\+([\w\d]+(?:\.[\w\d]+)*))?""".r

    def parseVersion(version: String): (Int, Int, Int, Option[String]) = version match {
      case regex(major, minor, patch, preRelease, _) =>
        (major.toInt, Option(minor).getOrElse("0").toInt, Option(patch).getOrElse("0").toInt, Option(preRelease))
      case _ => throw new IllegalArgumentException("Invalid version format")
    }

    val (major1, minor1, patch1, preRelease1) = parseVersion(version1)
    val (major2, minor2, patch2, preRelease2) = parseVersion(version2)

    if (major1 != major2) {
      major1.compareTo(major2)
    } else if (minor1 != minor2) {
      minor1.compareTo(minor2)
    } else if (patch1 != patch2) {
      patch1.compareTo(patch2)
    } else {
      // Compare pre-release versions if present
      val preReleaseComparison = (preRelease1, preRelease2) match {
        case (Some(pre1), Some(pre2)) => pre1.compareTo(pre2)
        case (Some(_), None) => -1
        case (None, Some(_)) => 1
        case (None, None) => 0
      }

      preReleaseComparison
    }
  }

}
