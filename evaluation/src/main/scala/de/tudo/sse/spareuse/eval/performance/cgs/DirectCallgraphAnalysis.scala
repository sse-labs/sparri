package de.tudo.sse.spareuse.eval.performance.cgs
import com.typesafe.config.ConfigValueFactory
import de.tudo.sse.spareuse.core.maven.{MavenIdentifier, MavenJarDownloader}
import org.opalj.br.BaseConfig
import org.opalj.br.analyses.Project
import org.opalj.log.GlobalLogContext
import org.opalj.tac.cg.{CallGraph, RTACallGraphKey, XTACallGraphKey}

import java.io.{ByteArrayOutputStream, File, InputStream}
import java.net.URL
import java.nio.file.{Files, StandardOpenOption}
import scala.collection.mutable
import scala.util.Try

class DirectCallgraphAnalysis extends WholeProgramCgAnalysis {

  private var dependencyJarFiles = new mutable.HashSet[File]
  private var rootJarFile: Option[File] = None

  override def prepareData(rootGav: String, dependencyGavs: Set[String]): Try[Unit] = Try {
    val dirPath = Files.createTempDirectory("spar-eval")

    val downloader = new MavenJarDownloader

    def downloadJar(jarIdent: MavenIdentifier): Try[File] = {
      downloader.downloadJar(jarIdent).map{ moj =>
        val is = moj.content
        val jarBytes = getBytes(is)
        val jarPath = dirPath.resolve(jarIdent.artifactId + "-" + jarIdent.version + ".jar")
        Files.createFile(jarPath)
        Files.write(jarPath, jarBytes, StandardOpenOption.WRITE)
        logger.debug(s"Successfully stored JAR ${jarIdent.toString} at ${jarPath.toString}")
        jarPath.toFile
      }
    }

    dependencyGavs
      .map(MavenIdentifier.fromGAV)
      .map(identOpt => downloadJar(identOpt.get))
      .foreach(fileT => dependencyJarFiles.add(fileT.get))

    rootJarFile = Some(downloadJar(MavenIdentifier.fromGAV(rootGav).get).get)

    downloader.shutdown()

  }

  override def buildFullCallgraph(): Try[CallGraph] = Try {
    logger.info("Building OPAL project...")
    val opalProject: Project[URL] = Project(Array(rootJarFile.get), dependencyJarFiles.toArray, logContext = GlobalLogContext, config = BaseConfig.withValue("org.opalj.br.analyses.cg.InitialEntryPointsKey.analysis",
      ConfigValueFactory.fromAnyRef("org.opalj.br.analyses.cg.LibraryEntryPointsFinder")))

    logger.info("Building RTA Callgraph for OPAl project....")
    val cg = opalProject.get(RTACallGraphKey)
    logger.info("Done building RTA Callgraph.")

    logger.info(s"#nodes: ${cg.reachableMethods().size} #edges: ${cg.numEdges}")

    cg
  }

  override def cleanup(): Unit = {
    dependencyJarFiles.foreach{ f => Try(f.delete()) }
    dependencyJarFiles = new mutable.HashSet[File]()

    rootJarFile.foreach{ f => Try(f.delete()) }
    rootJarFile = None
  }


  private def getBytes(in: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val buffer = new Array[Byte](32 * 1024)

    Stream.continually(in.read(buffer)).takeWhile(_ > 0).foreach { bytesRead =>
      baos.write(buffer, 0, bytesRead)
      baos.flush()
    }

    baos.toByteArray

  }
}
