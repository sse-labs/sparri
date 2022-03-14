package de.tudo.sse.classfilefeatures.common.opal

import com.typesafe.config.ConfigValueFactory
import OPALProjectHelper.ClassList
import org.opalj.br.analyses.{InconsistentProjectException, Project}
import org.opalj.br.reader.Java16LibraryFramework
import org.opalj.br.{BaseConfig, ClassFile}
import org.opalj.bytecode.JRELibraryFolder
import org.opalj.log.{GlobalLogContext, LogContext, OPALLogger, StandardLogContext}
import org.slf4j.{Logger, LoggerFactory}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, File, FileInputStream, InputStream}
import java.net.URL
import java.util.jar.JarInputStream
import java.util.zip.ZipFile
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Try}

object OPALProjectHelper {
  type ClassList = List[(ClassFile, URL)]
}

class OPALProjectHelper(projectLogger: OPALLogger = new WarnOnlyLogger(OPALProjectHelper.getClass),
                        loadJreClassImplementation: Boolean = true) {

  private val projectLogCtx: LogContext = {
    val ctx = new StandardLogContext()
    OPALLogger.register(ctx, projectLogger)
    OPALLogger.updateLogger(GlobalLogContext, projectLogger)
    ctx
  }
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val jreLibraryFolder = {
    val propValue = System.getProperty("analysisJre")

    if(propValue != null && propValue.nonEmpty) {
      log.info(s"Loading user-defined JRE for analysis: $propValue")
      new File(propValue)
    } else {
      JRELibraryFolder
    }
  }

  private lazy val jreClassFqns: List[String] = jreClasses.map(_._1.fqn)

  private lazy val jreClasses: ClassList = {

    def getJarFilesRecursive(directory: File): List[File] = {
      val directChildJars = directory
        .listFiles
        .filter(f => f.isFile && f.getName.toLowerCase().endsWith(".jmod") || f.getName.toLowerCase().endsWith(".jar"))
        .toList
      directChildJars ++ directory.listFiles.filter(_.isDirectory).flatMap(getJarFilesRecursive).toList
    }
    getJarFilesRecursive(jreLibraryFolder)
      .filter(f => !f.getName.equals("jfxswt.jar")) // Do not load SWT classes, they depend on eclipse implementations
      .map(f => {
        if(f.getName.endsWith(".jmod"))
          readClassesFromJmodFile(f, loadJreClassImplementation)
        else
          readClassesFromJarStream(new FileInputStream(f), f.toURI.toURL, loadJreClassImplementation)
      })
      .filter(readTry => readTry match {
        case Failure(ex) =>
          log.error("Failed to load JRE library file: " + ex.getMessage)
          false
        case _ => true
      })
      .flatMap(_.get)
  }

  /**
   * Builds an OPAL Project representing the current JRE, without any additional dependencies.
   * @return Initialized OPAL Project for the JRE
   */
  def buildJreOPALProject(): Project[URL] = {
    val config = BaseConfig.withValue("org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
      ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryEntryPointsFinder"))

    val inconsistentExceptionHandler =
      (_: LogContext, error: InconsistentProjectException) => log.warn("Inconsistent Project Exception: " + error.message)

    Project(jreClasses,
      List.empty,
      libraryClassFilesAreInterfacesOnly = !loadJreClassImplementation,
      Traversable.empty,
      inconsistentExceptionHandler
    )(config, projectLogger)
  }

  /**
   * Builds an OPAL Project instance representing the given project classes with the given third party dependency classes.
   * @param projectClasses List of Classfiles representing the project
   * @param thirdPartyClasses List of Classfiles representing the project's dependencies
   * @param loadJre Switch indicating whether or not the JRE classes should be considered as dependencies. Defaults to true
   * @return Initialized OPAL project for the given classes
   */
  def buildOPALProject(projectClasses: ClassList, thirdPartyClasses: ClassList, loadJre: Boolean = true): Project[URL] = {

    val config = BaseConfig.withValue("org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
      ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryEntryPointsFinder"))

    val dependencies = if(loadJre) { thirdPartyClasses ++ jreClasses } else thirdPartyClasses

    val inconsistentExceptionHandler =
      (_: LogContext, error: InconsistentProjectException) => log.warn("Inconsistent Project Exception: " + error.message)

    Project(projectClasses,
      dependencies,
      libraryClassFilesAreInterfacesOnly = if(loadJre) !loadJreClassImplementation else false, // IMPROVE: Make this configurable
      Traversable.empty,
      inconsistentExceptionHandler
    )(config, projectLogger)
  }

  /**
   * Processes a given JAR stream and creates a list of ClassFiles contained within
   * @param jarStream InputStream representing a JAR file.
   * @param source The source URL that will be associated with all resulting class files. Usually location of the JAR file.
   * @param loadImplementation Switch indicating whether or not the actual implementation of the classes should be loaded.
   * @return Try object holding the list of classfiles, or a Failure with additional information.
   */
  def readClassesFromJarStream(jarStream: InputStream, source:URL, loadImplementation: Boolean): Try[ClassList] = Try {

    val entries = new ListBuffer[(ClassFile, URL)]()
    val jarInputStream = new JarInputStream(jarStream)

    var currentEntry = jarInputStream.getNextJarEntry

    val reader = getClassFileReader(loadImplementation)

    while(currentEntry != null){
      val entryName = currentEntry.getName.toLowerCase

      if (entryName.endsWith(".class")){

        reader
          .ClassFile(getEntryByteStream(jarInputStream))
          .map(cf => (cf, source))
          .foreach(t => entries.append(t))
      }

      currentEntry = jarInputStream.getNextJarEntry
    }

    entries.toList
  }

  /**
   * Frees up resources occupied by OPAL.
   */
  def freeOpalResources(): Unit = {
    //TODO: We will have to adapt OPAL so that the type caches are being cleared, otherwise we will slowly run out of
    //TODO: memory while analyzing multiple projects
    log.warn("freeOpalResources was called, but is currently not implemented")
  }

  /**
   * Builds the ClassFileReader to process ClassFiles with.
   * @param loadImplementation Decides whether or not the reader loads implementation (code) or interfaces-only
   * @return ClassFileReader
   * @note We do not use instance variables here, since the Java16LibraryFramework uses caching, and the caches will
   *       grow continuously until the class space is exhausted
   */
  private def getClassFileReader(loadImplementation: Boolean): Java16LibraryFramework = {
    if(loadImplementation)
      Project.JavaClassFileReader(projectLogCtx, BaseConfig)
    else
      Java16LibraryFramework
  }

  private def readClassesFromJmodFile(jmod: File, loadImplementation: Boolean): Try[ClassList] = Try {
    val entries = new ListBuffer[(ClassFile, URL)]()

    val zipFile = new ZipFile(jmod)
    val source = jmod.toURI.toURL
    val entryEnum = zipFile.entries()

    val reader = getClassFileReader(loadImplementation)

    while(entryEnum.hasMoreElements){
      val currentEntry = entryEnum.nextElement()
      val entryName = currentEntry.getName.toLowerCase

      if (entryName.endsWith(".class")){
        val is = zipFile.getInputStream(currentEntry)

        reader
          .ClassFile(getEntryByteStream(is))
          .map(cf => (cf, source))
          .foreach(t => entries.append(t))
      }
    }

    entries.toList
  }

  private def getEntryByteStream(in: InputStream): DataInputStream = {
    val entryBytes = {
      val baos = new ByteArrayOutputStream()
      val buffer = new Array[Byte](32 * 1024)

      Stream.continually(in.read(buffer)).takeWhile(_ > 0).foreach { bytesRead =>
        baos.write(buffer, 0, bytesRead)
        baos.flush()
      }

      baos.toByteArray
    }

    new DataInputStream(new ByteArrayInputStream(entryBytes))
  }
}
