package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaMethod, JavaProgram}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.execution.analyses.{AnalysisImplementationDescriptor, AnalysisResult, ExistingResult, IncrementalAnalysisImplementation}
import org.opalj.ai.domain
import org.opalj.ai.fpcf.properties.AIDomainFactoryKey
import org.opalj.br.Method
import org.opalj.tac.ComputeTACAIKey

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

abstract class DefaultIFDSSummaryBuilder(baselineRunOpt: Option[AnalysisRunData]) extends IncrementalAnalysisImplementation(baselineRunOpt) {

  // Information to customize analysis descriptor for concrete IFDS Summary Builders
  protected val analysisName: String
  protected val analysisVersion: String
  protected val analysisDescription: String

  protected val opalHelper = new OPALProjectHelper(loadJreClassImplementation = false)

  private var squashStatements = true


  override val descriptor: AnalysisImplementationDescriptor = DefaultIFDSSummaryBuilder.buildDescriptor(analysisName, analysisVersion, analysisDescription)

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {
    if (inputs.exists(e => !e.isInstanceOf[JavaProgram])) {
      log.warn(s"Execution of analysis ${descriptor.fullName} not possible: Inputs must be of kind 'Program'")
      false
    } else {
      if (rawConfig.isBlank) true
      else {
        if (rawConfig.equalsIgnoreCase("--keep-identity-stmts")) true
        else {
          log.warn(s"Unknown configuration options $rawConfig")
          false
        }
      }
    }
  }

  override def executeIncremental(input: SoftwareEntityData, previousResults: Set[AnalysisResultData], rawConfig: String): Try[Set[AnalysisResult]] = {
    this.squashStatements = !rawConfig.trim.equalsIgnoreCase("--keep-identity-stmts")

    def findValidPreviousResult(method: Method): Option[AnalysisResultData] = {
      val methodHash = OPALJavaConverter.buildMethodHash(method)
      previousResults
        .find{ r =>
          r.affectedEntities.exists{
            case jm: JavaMethod =>
              methodHash == jm.methodHash
            case _ => false
          }
        }
    }

    getFileFor(input) match {
      case Success(inputStream) =>
        Try{

          log.info(s"Obtaining OPAL project instance for input ${input.name} ...")

          val jarUrl = MavenIdentifier.fromGAV(input.name).map(_.toJarLocation.toURL).get
          val classes = opalHelper.readClassesFromJarStream(inputStream, jarUrl, loadImplementation = true).get
          val project = opalHelper.buildOPALProject(classes, List.empty, loadJre = false, setLibraryMode = true)

          log.info("Project initialized.")

          // Use simplest AI domain for TAC
          project.updateProjectInformationKeyInitializationData(AIDomainFactoryKey) {
            case None => Set(classOf[domain.RecordDefUse])
            case Some(requirements) => requirements + classOf[domain.RecordDefUse]
          }

          // Mapping of methods to their TAC
          implicit val TACAIProvider: MethodTACProvider = project.get(ComputeTACAIKey)

          // Detect changedMethods / new methods that need full graph recomputations

          val previousResultsToLink = project
            .allMethodsWithBody
            .flatMap(findValidPreviousResult)
            .map(r => ExistingResult(r.uid))

          val totalMethodCnt = project.allMethodsWithBody.size
          val unchangedMethodCnt = previousResultsToLink.size
          val computationsNeeded = totalMethodCnt - unchangedMethodCnt

          log.info(s"Found $totalMethodCnt methods with bodies, $unchangedMethodCnt results can be reused.")

          var methodCnt = 0

          val allGraphs = project
            .allMethodsWithBody
            .filterNot(m => findValidPreviousResult(m).isDefined)
            .map{ m =>
              log.info(s"\t [ $methodCnt / $computationsNeeded ] Building IFDS summary for method: ${m.toJava}")
              methodCnt += 1
              analyzeMethod(m)
            }

          //TODO: Handle results

        }

      case Failure(ex) =>
        log.error(s"Failed to build IFDS summaries for entity ${input.name}, JAR download failed", ex)
        Failure(ex)

    }


    opalHelper.freeOpalResources()

    ???
  }




  /**
   * This method iterates the TAC-representation of a method body and iteratively creates an IFDSMethodGraph. It invokes
   * analyzeStatement() for every statement node at most one time, and makes sure the successor- and predecessor-relations
   * between statement nodes a set correctly. AnalyzeStatement() has to set all activations for each statement node accordingly.
   * @param method The method to build an IFDS graph for
   * @param TACAIProvider A TAC provider to get the three-address-code representation of a methods body
   * @return Finalized IFDSMethodGraph that contains all possible fact activations
   */
  protected def analyzeMethod(method: Method)(implicit TACAIProvider: MethodTACProvider): IFDSMethodGraph = {

    val theTAC = TACAIProvider(method)
    val cfg = theTAC.cfg
    val graph = new IFDSMethodGraph(method)

    val firstStmt = cfg.code.instructions(theTAC.pcToIndex(0))
    val firstNode = graph.createStatement(firstStmt, None)

    val workList = new ListBuffer[StatementNode]
    workList.append(firstNode)

    while (workList.nonEmpty) {

      val currentNode = workList.remove(0)
      val stmtIdx = theTAC.pcToIndex(currentNode.stmt.pc)

      analyzeStatement(currentNode, method, graph)

      cfg
        .foreachSuccessor(stmtIdx) { successorIdx =>
          val successorStmt = cfg.code.instructions(successorIdx)

          val isProcessed = graph.hasStatement(successorStmt.pc)

          if (isProcessed) {
            graph
              .getStatement(successorStmt.pc)
              .get
              .addPredecessor(currentNode)
          } else {
            val successorNode = graph.createStatement(successorStmt, Some(currentNode))
            workList.append(successorNode)
          }
        }
    }

    graph
  }

  protected def analyzeStatement(currentNode: StatementNode, currentMethod: Method, graph: IFDSMethodGraph): Unit

}

object DefaultIFDSSummaryBuilder {

  def buildDescriptor(aName: String, aVersion: String, aDescription: String): AnalysisImplementationDescriptor = new AnalysisImplementationDescriptor {

    override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Method

    override val analysisData: AnalysisData = AnalysisData.systemAnalysis(
      aName,
      aVersion,
      aDescription,
      "OPAL 4.0.0",
      Set("java", "scala"),
      ???, //TODO: Define general format for IFDS Method flow graphs
      SoftwareEntityKind.Program,
      doesBatchProcessing = true,
      isIncremental = true
      )
  }

}
