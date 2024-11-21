package org.anon.spareuse.client.analyses

import com.typesafe.config.{Config, ConfigFactory}
import org.opalj.br.analyses.Project
import org.opalj.bytecode.RTJar

import java.io.File
import java.net.URL
import java.nio.file.Path

abstract class LocalMavenClientAnalysis[T](mavenProjectDir: Path) extends ClientAnalysis[T](
  classFilesDirectory = mavenProjectDir.resolve("target").resolve("classes").toFile,
  pomFile = mavenProjectDir.resolve("pom.xml").toFile
) {

  private final val extraLibrariesDirKey = "sparri.client.lib-folder"
  private final val loadLibrariesKey = "sparri.client.load-lib-contents"
  private final val loadJreKey = "sparri.client.load-jre"

  protected[analyses] val mavenRoot: File = mavenProjectDir.toFile

  protected[analyses] lazy val projectConfig: Option[Config] = {
    val configFile = mavenProjectDir.resolve(".sparri").toFile

    if(configFile.exists() && configFile.isFile){
      Some(ConfigFactory.parseFile(configFile))
    } else None
  }

  override def getOpalProject(loadJre: Boolean): Project[URL] = {
    val projectCfs = Project.JavaClassFileReader.AllClassFiles(Seq(classFilesDirectory))

    def getConfigValue(key: String): Option[String] = projectConfig.flatMap { conf => if(conf.hasPath(key)) Some(conf.getString(key)) else None }

    val fullyLoadLibraries = getConfigValue(loadLibrariesKey).exists(value => value.equalsIgnoreCase("true"))
    val loadJre = getConfigValue(loadJreKey).exists(value => value.equalsIgnoreCase("true"))

    val libraryCfs = getConfigValue(extraLibrariesDirKey) match {
      case Some(libDir) if libDir.nonEmpty =>
        val librariesDir = mavenProjectDir.resolve(libDir).toFile
        if(librariesDir.exists() && librariesDir.isDirectory){
          val filesToLoad = if(loadJre) Seq(RTJar, librariesDir) else Seq(librariesDir)

          if(fullyLoadLibraries) Project.JavaClassFileReader.AllClassFiles(filesToLoad)
          else Project.JavaLibraryClassFileReader.AllClassFiles(filesToLoad)
        } else {
          log.warn(s"Invalid configuration: $extraLibrariesDirKey must point to a valid directory.")
          log.warn("Not loading any additional libraries.")
          Seq.empty
        }

      case Some(_) =>
        log.warn(s"Invalid configuration supplied for $extraLibrariesDirKey")
        log.warn("Not loading any additional libraries.")
        Seq.empty

      case None =>
        Seq.empty
    }

    Project(projectCfs, libraryCfs, libraryClassFilesAreInterfacesOnly = !fullyLoadLibraries)
  }

}
