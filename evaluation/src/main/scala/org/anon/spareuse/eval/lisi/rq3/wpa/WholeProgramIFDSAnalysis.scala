package org.anon.spareuse.eval.lisi.rq3.wpa

import org.anon.spareuse.client.analyses.LocalMavenClientAnalysis
import org.anon.spareuse.core.maven.MavenJarDownloader
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.execution.analyses.impl.cg
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.{DefinedMethod, MethodIdent}
import org.anon.spareuse.execution.analyses.impl.ifds.reachability.{IFDSMethodRunner, IFDSRunnerEnvironment}
import org.anon.spareuse.execution.analyses.impl.ifds.{IFDSTaintFlowSummaryBuilderImpl, IFDSZeroFact, MethodTACProvider}
import org.opalj.ai.domain
import org.opalj.ai.fpcf.properties.AIDomainFactoryKey
import org.opalj.br.DeclaredMethod
import org.opalj.br.analyses.Project
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

  override def execute(): Try[Int] = {
    log.info(s"Analyzing Maven project at ${mavenDir.toAbsolutePath.toString}")
    log.info(s"Downloading project dependencies...")
    buildLibDir()
      .map { libraryDir =>
        log.info(s"Initializing OPAL project ...")
        val opalProject = initOPALProject(libraryDir)
        log.info(s"Successfully obtained OPAL project instance.")

        log.info(s"Building RTA call graph for whole program...")
        val theCg = opalProject.get(RTACallGraphKey)
        log.info(s"Successfully obtained RTA call graph with ${theCg.reachableMethods().size} reachable methods.")

        log.info(s"Building IFDS summaries ...")
        // Mapping of methods to their TAC
        implicit val TACAIProvider: MethodTACProvider = opalProject.get(ComputeTACAIKey)

        val summaryDict = theCg
          .reachableMethods()
          .flatMap { ctx =>
            if(ctx.method.hasSingleDefinedMethod){
              val definedMethod = ctx.method.asDefinedMethod.definedMethod
              Some(ifdsSummaryBuilder.analyzeMethod(definedMethod))
            } else {
              None
            }
          }
          .map(graph => (graph.methodIdentifier, graph))
          .toMap

        log.info(s"Done building IFDS summaries for ${summaryDict.size} reachable methods with body.")

        // We need to convert the OPAL CG into our internal representation so the IFDS solver can work with it
        val cgInternal = new cg.CallGraph{

          private val opalMethodLookup: Map[MethodIdent, DeclaredMethod] = theCg
            .reachableMethods()
            .map{ ctx =>
              val ident = MethodIdent(ctx.method.declaringClassType.fqn, ctx.method.name, ctx.method.descriptor.toJVMDescriptor)
              (ident, ctx.method)
            }
            .toMap

          private val identLookup: Map[DeclaredMethod, MethodIdent] = opalMethodLookup.map{ case (key, value) => (value, key) }

          private val methodLookup: Map[MethodIdent, CallGraphBuilder.DefinedMethod] = opalMethodLookup
            .keys
            .map{ ident =>
              val opalMethod = opalMethodLookup(ident)
              val isStatic = if(opalMethod.hasSingleDefinedMethod) opalMethod.asDefinedMethod.definedMethod.isStatic else false
              val defM = new CallGraphBuilder.DefinedMethod(ident, isStatic, () => List.empty, () => List.empty)
              (ident, defM)
            }
            .toMap

          override def lookupMethod(ident: MethodIdent): CallGraphBuilder.DefinedMethod = methodLookup(ident)

          override def reachableMethods(): Set[CallGraphBuilder.DefinedMethod] = methodLookup.values.toSet

          override def calleesOf(dm: CallGraphBuilder.DefinedMethod): Iterable[(Int, Set[CallGraphBuilder.DefinedMethod])] = {
            if(!opalMethodLookup.contains(dm.methodIdentifier)){
              Iterable.empty
            } else {
              val opalMethod = opalMethodLookup(dm.methodIdentifier)
              theCg
                .calleesOf(opalMethod)
                .map{ case (pc, targetIt) =>
                  val internalTargets = targetIt.map(ctx => methodLookup(identLookup(ctx.method))).toSet
                  (pc, internalTargets)
                }.toSet
            }
          }

          override def calleesOf(dm: CallGraphBuilder.DefinedMethod, pc: Int): Set[CallGraphBuilder.DefinedMethod] = {
            if(!opalMethodLookup.contains(dm.methodIdentifier)){
              Set.empty
            } else {
              val opalMethod = opalMethodLookup(dm.methodIdentifier)
              theCg
                .calleesOf(opalMethod, pc)
                .map { ctx =>
                  methodLookup(identLookup(ctx.method))
                }
                .toSet
            }
          }

          override def callersOf(dm: CallGraphBuilder.DefinedMethod): Set[CallGraphBuilder.DefinedMethod] = {
            if(!opalMethodLookup.contains(dm.methodIdentifier)){
              Set.empty
            } else {
              val opalMethod = opalMethodLookup(dm.methodIdentifier)
              theCg
                .callersOf(opalMethod)
                .iterator
                .map { case (caller, _, _) =>
                  methodLookup(identLookup(caller))
                }
                .toSet
            }
          }
        }

        val ifdsRunner = new IFDSMethodRunner(IFDSRunnerEnvironment(cgInternal, summaryDict))

        var entryCnt = 0

        getLibraryEntryPoints(theCg, opalProject)
          .foreach{ libEntry =>
            log.info(s"Processing entry point #$entryCnt: ${libEntry.toJava}")
            if(libEntry.hasSingleDefinedMethod){
              val entryMethod = libEntry.asDefinedMethod.definedMethod
              val ident = MethodIdent(entryMethod.classFile.fqn, entryMethod.name, entryMethod.descriptor.toJVMDescriptor)

              val resultingFacts = ifdsRunner.resolveFrom(ident, Set(IFDSZeroFact))
              log.info(s"Running entry ${ident.toString} got fact: ${resultingFacts.map(_.displayName).mkString}")
            }
            entryCnt += 1
          }
        0
      }

  }

  // This analysis is a traditional WPA IFDS analysis, it does not need any precomputations
  override final def requirements: Seq[AnalysisRequirement] = Seq.empty[AnalysisRequirement]

  private def initOPALProject(libDir: Path): Project[URL] = {
    val projectCfs = Project.JavaClassFileReader.AllClassFiles(Seq(classFilesDirectory))
    val libCfs = Project.JavaClassFileReader.AllClassFiles(Seq(libDir.toFile, RTJar))

    log.info(s"Loaded ${projectCfs.size} project class files and ${libCfs.size} library class files")

    val project = Project(projectCfs, libCfs, libraryClassFilesAreInterfacesOnly = false)

    // Use simplest AI domain for TAC
    project.updateProjectInformationKeyInitializationData(AIDomainFactoryKey) {
      case None => Set(classOf[domain.RecordDefUse])
      case Some(requirements) => requirements + classOf[domain.RecordDefUse]
    }

    project
  }

  private def getLibraryEntryPoints(cg: CallGraph, project: Project[URL]): Set[DeclaredMethod] = {

    cg
      .reachableMethods()
      .flatMap(ctx => cg.calleesOf(ctx.method).flatMap(t => t._2.map(t2 => (ctx, t._1, t2))))
      .filter{
        case (caller, _, callee) =>
          !project.isProjectType(callee.method.declaringClassType) && project.isProjectType(caller.method.declaringClassType) && !callee.method.declaringClassType.fqn.startsWith("java")
      }.map{
        case (_, _, callee) =>
          callee.method
      }
      .toSet
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
    theAnalysis.execute() match {
      case Success(result) =>
        log.info(s"Analysis finished successfully")
        System.exit(result)
      case Failure(ex) =>
        log.error(s"Failure while running analysis", ex)
        System.exit(1)
    }
  }

}
