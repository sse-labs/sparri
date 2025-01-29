package org.anon.spareuse.eval.lisi.rq3.wpa

import org.anon.spareuse.client.analyses.LocalMavenClientAnalysis
import org.anon.spareuse.core.maven.MavenJarDownloader
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.execution.analyses.impl.cg
import org.anon.spareuse.execution.analyses.impl.cg.{CallGraphBuilder, DefaultRTACallGraphBuilder, JreModelLoader}
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.{DefinedMethod, MethodIdent}
import org.anon.spareuse.execution.analyses.impl.ifds.reachability.{IFDSMethodRunner, IFDSRunnerEnvironment}
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSTaintFlowSummaryBuilderImpl, IFDSZeroFact, MethodTACProvider}
import org.opalj.ai.domain
import org.opalj.ai.fpcf.properties.AIDomainFactoryKey
import org.opalj.br.DeclaredMethod
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.cg.ApplicationEntryPointsFinder
import org.opalj.bytecode.RTJar
import org.opalj.tac.ComputeTACAIKey
import org.opalj.tac.cg.{CallGraph, RTACallGraphKey}
import org.slf4j.{Logger, LoggerFactory}

import java.net.URL
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class WholeProgramIFDSAnalysis(mavenDir: Path) extends LocalMavenClientAnalysis[Int](mavenDir) {

  private val ifdsSummaryBuilder = new IFDSTaintFlowSummaryBuilderImpl(None)

  private val jreDir: String = if(Files.exists(Paths.get("..", "jre-data"))) "../jre-data" else "jre-data"

  override def execute(arguments: Array[String]): Try[Int] = {
    log.info(s"Analyzing Maven project at ${mavenDir.toAbsolutePath.toString}")
    log.info(s"Downloading project dependencies...")
    buildLibDir()
      .map { libraryDir =>
        log.info(s"Initializing OPAL project ...")
        val opalProject = initOPALProject(libraryDir)
        log.info(s"Successfully obtained OPAL project instance.")

        log.info(s"Building RTA call graph for whole program...")
        JreModelLoader.indexJreData(jreDir)
        val projectRepresentation = OPALJavaConverter.convertProgram("local.project:1.0.0", "<default>",
          opalProject.allClassFiles.toList, "<NONE>")
        val cgBuilder = new DefaultRTACallGraphBuilder(Set(projectRepresentation), JreModelLoader.getDefaultJre.map(_.version).toOption)
        val entryPoints = ApplicationEntryPointsFinder
          .collectEntryPoints(opalProject)
          .filter(m => opalProject.isProjectType(m.classFile.thisType))
          .flatMap( epM => projectRepresentation.allMethods.find(m => m.name == epM.name && m.enclosingClass.get.thisType == epM.classFile.fqn && m.descriptor == epM.descriptor.toJVMDescriptor) )
          .map(cgBuilder.asDefinedMethod)



        entryPoints.zipWithIndex.foreach{ case (epM, idx) =>
          log.info(s"Processing entrypoint ${epM.methodIdentifier} ($idx/${entryPoints.size})")
          cgBuilder.buildFrom(epM)
        }

        val callGraph = cgBuilder.getGraph

        log.info(s"Successfully obtained RTA call graph with ${callGraph.reachableMethods().size} reachable methods.")


        log.info(s"Building IFDS summaries ...")
        // Mapping of methods to their TAC
        implicit val TACAIProvider: MethodTACProvider = opalProject.get(ComputeTACAIKey)
        val summaryDict = opalProject
          .allClassFiles
          .flatMap(_.methods)
          .map{ m =>
            val ident = MethodIdent(m.classFile.fqn, m.name, m.descriptor.toJVMDescriptor)
            (ident, ifdsSummaryBuilder.analyzeMethod(m))
          }
          .toMap

        log.info(s"Done building ${summaryDict.size} IFDS summaries")
        val ifdsRunner = new IFDSMethodRunner(IFDSRunnerEnvironment(callGraph, summaryDict))

        log.info(s"Running IFDS Queries...")

        entryPoints.zipWithIndex.foreach{ case (epM, idx) =>
          log.info(s"Running IFDS solver for method ${epM.methodIdentifier.toString} ($idx / ${entryPoints.size})")
          val resultingFacts = ifdsRunner.resolveFrom(epM.methodIdentifier, Set(IFDSZeroFact))
          log.info(s"Running entry ${epM.methodIdentifier.toString} got fact: ${resultingFacts.map(_.displayName).mkString}")
        }
        0
      }

  }

  // This analysis is a traditional WPA IFDS analysis, it does not need any precomputations
  override final def requirements: Seq[AnalysisRequirement] = Seq.empty[AnalysisRequirement]

  private def initOPALProject(libDir: Path): Project[URL] = {
    val projectCfs = Project.JavaClassFileReader.AllClassFiles(Seq(classFilesDirectory))
    val libCfs = Project.JavaClassFileReader.AllClassFiles(Seq(libDir.toFile))

    log.info(s"Loaded ${projectCfs.size} project class files and ${libCfs.size} library class files")

    val project = Project(projectCfs, libCfs, libraryClassFilesAreInterfacesOnly = false)

    // Use simplest AI domain for TAC
    project.updateProjectInformationKeyInitializationData(AIDomainFactoryKey) {
      case None => Set(classOf[domain.RecordDefUse])
      case Some(requirements) => requirements + classOf[domain.RecordDefUse]
    }

    project
  }


  private def buildLibDir(): Try[Path] = {
    val downloader = new MavenJarDownloader()

    val theDirectory = Files.createTempDirectory("sparri-analysis")

    val result = getAllDependencies
      .map{ dependencyList =>
        dependencyList
          .foreach{ dependency =>
            downloader.downloadJar(dependency.identifier) match {
              case Success(jarHandle) =>
                val fileName = s"${dependency.identifier.artifactId.replace(".", "_")}__${dependency.identifier.version.replace(".", "_")}.jar"
                Try (Files.copy(jarHandle.content, theDirectory.resolve(fileName))) match {
                  case Success(_) =>
                    log.info(s"Successfully downloaded dependency ${dependency.identifier.toString}")
                    jarHandle.content.close()
                  case Failure(ex) =>
                    log.error(s"Failed to write dependency ${dependency.identifier.toString} to file", ex)
                    jarHandle.content.close()
                }
              case Failure(ex) =>
                log.error(s"Failed to download dependency ${dependency.identifier.toString}", ex)
            }
          }

        theDirectory
      }

    downloader.shutdown()

    result
  }
}

object WholeProgramIFDSAnalysisRunner  {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {
    if(args.length != 1){
      log.error(s"Usage: WholeProgramIFDSAnalysisRunner <maven-project-dir>")
      System.exit(-1)
    }

    val theAnalysis = new WholeProgramIFDSAnalysis(Paths.get(args(0)))
    theAnalysis.initialize()
    theAnalysis.execute(Array.empty[String]) match {
      case Success(result) =>
        log.info(s"Analysis finished successfully")
        System.exit(result)
      case Failure(ex) =>
        log.error(s"Failure while running analysis", ex)
        System.exit(1)
    }
  }

}
