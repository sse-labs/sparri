package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.slf4j.{Logger, LoggerFactory}
import spray.json.JsObject

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.{Failure, Success, Try}


trait JREModelSerializer {

  private final val log: Logger = LoggerFactory.getLogger(getClass)
  private final val opalHelper: OPALProjectHelper = new OPALProjectHelper()

  protected[entities] def serializeModelTo(theModel: JavaProgram, outFile: File): Try[File] = Try {

    val json = modelToJson(theModel)
    val jsonString = json.prettyPrint

    Files.writeString(outFile.toPath, jsonString, StandardOpenOption.CREATE_NEW)

    outFile
  }

  protected[entities] def modelToJson(model: JavaProgram): JsObject

  def serializeJreModel(jreRoot: File, jreVersion: String, outDir: File): Try[File] = Try {

    if (!jreRoot.exists() || !jreRoot.isDirectory)
      throw new IllegalArgumentException(s"Not a valid directory: ${jreRoot.getAbsolutePath}")

    if (!outDir.exists() || !outDir.isDirectory)
      throw new IllegalArgumentException(s"Not a valid output directory: ${outDir.getAbsolutePath}")

    val outFile = Paths.get(outDir.getPath, s"jre-$jreVersion.json").toFile

    if (outFile.exists())
      throw new IllegalStateException(s"There already is a serialized object model for JRE $jreVersion")

    val relevantFiles = getJarFilesRecursive(jreRoot)

    if (relevantFiles.isEmpty)
      throw new IllegalArgumentException(s"No JARs / JMODs contained in JRE root: ${jreRoot.getAbsolutePath}")

    log.info(s"Parsing ${relevantFiles.size} JAR files for JRE $jreVersion.")

    val allJreClasses = relevantFiles
      .map { containerFile =>
        if (containerFile.getName.endsWith(".jar"))
          opalHelper.readClassesFromJarStream(new FileInputStream(containerFile), containerFile.toURI.toURL, loadImplementation = true)
        else
          opalHelper.readClassesFromJmodFile(containerFile, loadImplementation = true)
      }
      .filter {
        case Success(_) => true
        case Failure(ex) =>
          log.error("Failed to load a JRE file", ex)
          false
      }
      .flatMap(_.get)

    val jreModel = OPALJavaConverter.convertProgram(s"<none>:<JRE>:$jreVersion", "<none>", allJreClasses.map(_._1))
    opalHelper.freeOpalResources()

    log.info("Done creating object model for JRE. Writing model to file ... ")

    serializeModelTo(jreModel, outFile) match {
      case Success(outFile) =>
        log.info(s"Successfully wrote JRE model to file: ${outFile.getName}")
        outFile
      case Failure(ex) =>
        log.error("Failed to write JRE model to file", ex)
        throw ex
    }
  }

  protected def getJarFilesRecursive(directory: File): List[File] = {
    val directChildJars = directory
      .listFiles
      .filter(f => f.isFile && f.getName.toLowerCase().endsWith("java.base.jmod") || f.getName.toLowerCase().equals("rt.jar"))
      .toList
    directChildJars ++ directory.listFiles.filter(_.isDirectory).flatMap(getJarFilesRecursive).toList
  }

}

