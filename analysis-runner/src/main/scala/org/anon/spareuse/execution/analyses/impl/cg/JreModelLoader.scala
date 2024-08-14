package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.execution.AnalysisRunnerConfig
import org.opalj.bytecode.JRELibraryFolder
import org.slf4j.{Logger, LoggerFactory}

import java.io.{File, FileFilter}
import java.nio.file.Files
import scala.util.{Failure, Success, Try}
import spray.json.enrichString

object JreModelLoader extends JreRepresentationJsonSupport {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private[cg] var jreVersionMap: Map[String, JreData] = Map.empty
  private[cg] var defaultJre: String = "8"

  def indexJreData(jreDataDir: String): Unit = {
    val dataDir = new File(jreDataDir)

    if(!dataDir.exists() || !dataDir.isDirectory)
      throw new IllegalArgumentException(s"Not a valid JRE data directory: ${dataDir.getName}")

    jreVersionMap = dataDir
      .listFiles(new FileFilter {
        override def accept(pathname: File): Boolean = pathname.getName.endsWith(".json") && pathname.getName.startsWith("jre-")
      })
      .map{ jreFile => new JreData(jreFile) }
      .map( data => (data.version, data))
      .toMap

    if(jreVersionMap.nonEmpty)
      log.info(s"Successfully indexed ${jreVersionMap.size} JRE versions: ${jreVersionMap.keys.mkString(",")}")
    else {
      log.warn(s"No JRE versions in ${dataDir.getAbsolutePath}.")
      log.info(s"Using current JRE at ${JRELibraryFolder.getAbsolutePath} as default. Building model summary ...")

      RTA_JRE_Serializer.serializeJreModel(JRELibraryFolder, "<default>", dataDir) match {
        case Success(outFile) =>
          log.info("Successfully built JRE model summary.")
          jreVersionMap = Map("<default>" -> new JreData(outFile))
        case Failure(ex) =>
          log.error("Failed to construct default JRE summaries.")
          throw ex
      }
    }

    // If there is only one JRE, its the default one. If version 8 is not present, use any version as the fallback default
    if(jreVersionMap.size == 1 || !jreVersionMap.contains(defaultJre))
      defaultJre = jreVersionMap.keys.head

    log.info(s"The default JRE model will be JRE $defaultJre")
  }

  def getAllJreVersions: Set[String] = jreVersionMap.keys.toSet
  def hasJre(version: String): Boolean = jreVersionMap.contains(version)
  def getJre(version: String): Try[JreRepresentation] = jreVersionMap(version).representation
  def getDefaultJre: Try[JreRepresentation] = jreVersionMap(defaultJre).representation

  def clear(): Unit = jreVersionMap = Map.empty


  class JreData(jsonFile: File) {

    val version: String = jsonFile.getName.substring(4, jsonFile.getName.lastIndexOf("."))
    val file: File = jsonFile

    lazy val representation: Try[JreRepresentation] = Try {
      log.info(s"Loading JRE representation from ${file.getAbsolutePath}...")
      val strRep = Files.readString(file.toPath)
      val result = strRep.parseJson.convertTo[JreRepresentation](jreJsonFormat)
      log.info(s"Done loading representation for JRE $version.")
      result
    }

  }

}
