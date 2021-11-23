package org.tud.cgcrawling.opal

import com.typesafe.config.ConfigValueFactory
import org.opalj.br.analyses.{InconsistentProjectException, Project}
import org.opalj.br.reader.Java16LibraryFramework
import org.opalj.br.{BaseConfig, ClassFile, DeclaredMethod}
import org.opalj.bytecode.JRELibraryFolder
import org.opalj.log.{GlobalLogContext, LogContext, OPALLogger, StandardLogContext}
import org.slf4j.{Logger, LoggerFactory}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, File, FileInputStream, InputStream}
import java.net.URL
import java.util.jar.JarInputStream
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Try}

object OPALProjectHelper {

  type ClassList = List[(ClassFile, URL)]

  private val LOAD_JRE_IMPLEMENTATION = true

  private val projectLogger: OPALLogger = new WarnOnlyLogger(this.getClass)
  private val projectLogCtx: LogContext = {
    val ctx = new StandardLogContext()
    OPALLogger.register(ctx, projectLogger)
    OPALLogger.updateLogger(GlobalLogContext, projectLogger)
    ctx
  }

  private val log: Logger = LoggerFactory.getLogger(this.getClass)
  private val fullClassFileReader = Project.JavaClassFileReader(projectLogCtx, BaseConfig)
  private val interfaceClassFileReader = Java16LibraryFramework

  lazy val jreClassFqns: List[String] = jreClasses.map(_._1.fqn)

  lazy val jreClasses: ClassList = {

    def getJarFilesRecursive(directory: File): List[File] = {
      val directChildJars = directory.listFiles.filter(f => f.isFile && f.getName.toLowerCase().endsWith(".jar")).toList
      directChildJars ++ directory.listFiles.filter(_.isDirectory).flatMap(getJarFilesRecursive).toList
    }

    getJarFilesRecursive(JRELibraryFolder)
      .filter(f => !f.getName.equals("jfxswt.jar")) // Do not load SWT classes, they depend on eclipse implementations
      .map(f => {
        readClassesFromJarStream(new FileInputStream(f), f.toURI.toURL, LOAD_JRE_IMPLEMENTATION)
      })
      .filter(readTry => readTry match {
        case Failure(ex) =>
          log.error("Failed to load JRE library file: " + ex.getMessage)
          false
        case _ => true
      })
      .flatMap(_.get)
  }

  def isThirdPartyMethod(project: Project[URL],method: DeclaredMethod): Boolean = {
    !project.allProjectClassFiles.map(_.thisType).contains(method.declaringClassType) &&
      !jreClasses.map(_._1.thisType).contains(method.declaringClassType)
  }

  def isThirdPartyClassFile(project: Project[URL], cf: ClassFile): Boolean = {
    !project.allProjectClassFiles.contains(cf) && !jreClasses.map(_._1).contains(cf)
  }

  def buildOPALProject(projectClasses: ClassList, thirdPartyClasses: ClassList): Project[URL] = {

    val config = BaseConfig.withValue("org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
      ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryEntryPointsFinder"))

    val inconsistentExceptionHandler =
      (_: LogContext, error: InconsistentProjectException) => log.warn("Inconsistent Project Exception: " + error.message)

    Project(projectClasses, thirdPartyClasses ++ jreClasses, libraryClassFilesAreInterfacesOnly = false, Traversable.empty, inconsistentExceptionHandler)(config, projectLogger)
  }

  def readClassesFromJarStream(jarStream: InputStream, source:URL, loadImplementation: Boolean): Try[ClassList] = Try {

    val entries = new ListBuffer[(ClassFile, URL)]()
    val jarInputStream = new JarInputStream(jarStream)

    var currentEntry = jarInputStream.getNextJarEntry

    val reader = if(loadImplementation) this.fullClassFileReader else this.interfaceClassFileReader

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

  private def getEntryByteStream(in: JarInputStream): DataInputStream = {
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
