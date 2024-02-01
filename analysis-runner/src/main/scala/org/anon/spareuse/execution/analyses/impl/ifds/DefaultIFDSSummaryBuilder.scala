package org.anon.spareuse.execution.analyses.impl.ifds

import org.anon.spareuse.core.formats
import org.anon.spareuse.core.formats.{ListResultFormat, NamedPropertyFormat, ObjectResultFormat}
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaMethod, JavaProgram, buildMethodIdent}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.model.entities.conversion.OPALJavaConverter
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData, SoftwareEntityKind}
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.execution.analyses.impl.ifds.DefaultIFDSSummaryBuilder.{FactRep, InternalActivationRep, InternalVariableRep, MethodIFDSRep, StatementRep}
import org.anon.spareuse.execution.analyses.{AnalysisImplementationDescriptor, AnalysisResult, ExistingResult, FreshResult, IncrementalAnalysisImplementation}
import org.opalj.ai.domain
import org.opalj.ai.fpcf.properties.AIDomainFactoryKey
import org.opalj.br.Method
import org.opalj.tac.ComputeTACAIKey
import spray.json.{DefaultJsonProtocol, JsonFormat}

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
              methodHash == jm.methodHash &&
                method.descriptor.toJVMDescriptor == jm.descriptor &&
                method.name == jm.name
            case _ => false
          }
        }
    }

    val inputMethodMap = input
      .getChildren
      .flatMap( packageEnt => packageEnt.getChildren.flatMap( classEnt => classEnt.getChildren))
      .map(_.asInstanceOf[JavaMethod])
      .map(jm => (buildMethodIdent(jm.name, jm.descriptor), jm))
      .toMap

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

          val results = project
            .allMethodsWithBody
            .filterNot(m => findValidPreviousResult(m).isDefined)
            .map{ m =>
              log.info(s"\t [ $methodCnt / $computationsNeeded ] Building IFDS summary for method: ${m.toJava}")
              methodCnt += 1

              val ifdsGraph = analyzeMethod(m)
              val resultData = ifdsGraph.toResultRepresentation(squashStatements)

              val correspondingEntity = inputMethodMap.get(buildMethodIdent(m.name, m.descriptor.toJVMDescriptor))

              if(correspondingEntity.isEmpty) throw new IllegalStateException(s"Could not find defined method in input entities: ${m.toJava}")

              FreshResult(resultData, Set(correspondingEntity.get))
            }
            .toSet[AnalysisResult]

          log.info(s"Done building $methodCnt IFDS summaries. Freeing OPAL resources ..")
          opalHelper.freeOpalResources()
          results
        }

      case Failure(ex) =>
        log.error(s"Failed to build IFDS summaries for entity ${input.name}, JAR download failed", ex)
        opalHelper.freeOpalResources()
        Failure(ex)

    }

  }




  /**
   * This method iterates the TAC-representation of a method body and iteratively creates an IFDSMethodGraph. It invokes
   * analyzeStatement() for every statement node at most one time, and makes sure the successor- and predecessor-relations
   * between statement nodes a set correctly. AnalyzeStatement() has to set all activations for each statement node accordingly.
   * @param method The method to build an IFDS graph for
   * @param TACAIProvider A TAC provider to get the three-address-code representation of a methods body
   * @return Finalized IFDSMethodGraph that contains all possible fact activations
   */
  protected[ifds] def analyzeMethod(method: Method)(implicit TACAIProvider: MethodTACProvider): IFDSMethodGraph = {

    val theTAC = TACAIProvider(method)
    val cfg = theTAC.cfg
    val graph = IFDSMethodGraph(method)

    val firstStmt = cfg.code.instructions(theTAC.pcToIndex(0))
    val firstNode = graph.createStatement(firstStmt, None)

    val workList = new ListBuffer[StatementNode]
    workList.append(firstNode)

    while (workList.nonEmpty) {

      val currentNode = workList.remove(0)
      val stmtIdx = theTAC.pcToIndex(currentNode.stmtPc)
      val stmt = cfg.code.instructions(stmtIdx)

      analyzeStatement(currentNode, stmt, method, graph)

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

  protected[ifds] def analyzeStatement(currentNode: StatementNode, currentStatement: TACStmt, currentMethod: Method, graph: IFDSMethodGraph): Unit

}

object DefaultIFDSSummaryBuilder {

  private val variableFormat = new ObjectResultFormat(Set(
    NamedPropertyFormat("variableName", formats.StringFormat, "The (normalized) name of this local variable"),
    NamedPropertyFormat("defSites", ListResultFormat(formats.NumberFormat), "All definition sites of this local variable")
  ))

  case class InternalVariableRep(variableName: String, defSites: List[Int])

  private val activationFormat = new ObjectResultFormat(Set(
    NamedPropertyFormat("sourceFactId", formats.NumberFormat, "The uid of the fact that is being activated"),
    NamedPropertyFormat("enablingFactIds", ListResultFormat(formats.NumberFormat, "Id of a fact that may activate the source fact"), "List of facts activating the source fact")
  ))

  case class InternalActivationRep(sourceFactId: Int, enablingFactIds: List[Int])

  private val stmtFormat = new ObjectResultFormat(Set(
    NamedPropertyFormat("pc", formats.NumberFormat, "The program counter of this statement"),
    NamedPropertyFormat("isReturn", formats.NumberFormat, "Non-Zero if this statement is a return statement"),
    NamedPropertyFormat("TACRepresentation", formats.StringFormat, "String representation of the statement's Three-Address-Code"),
    NamedPropertyFormat("predecessors", ListResultFormat(formats.NumberFormat, "Program counters of predecessor statements"), "References to predecessors of this statement"),
    NamedPropertyFormat("calleeMethodName", formats.StringFormat, "Name of the method that is called by this statement, empty if no method is called."),
    NamedPropertyFormat("calleeDescriptor", formats.StringFormat, "String representation of the descriptor of the method that is called by this statement."),
    NamedPropertyFormat("calleeClassName", formats.StringFormat, "FQN of the declaring class of the method that is called by this statement"),
    NamedPropertyFormat("calleeParameterVariables", ListResultFormat(variableFormat), "The actual variables passed to the method that is called by this statement"),
    NamedPropertyFormat("callReceiverVar", variableFormat, "The variable representing the receiver of the method that is called by this statement"),
    NamedPropertyFormat("returnVariable", variableFormat, "The variable representing the value that is being returned if this statement is a return statement"),
    NamedPropertyFormat("activations", ListResultFormat(activationFormat, "Individual activation for this statement"))
  ))

  case class StatementRep(pc: Int,
                          isReturn: Int,
                          TACRepresentation: String,
                          predecessors: List[Int],
                          calleeMethodName: String,
                          calleeDescriptor: String,
                          calleeClassName: String,
                          calleeParameterVariables: List[InternalVariableRep],
                          callReceiverVar: InternalVariableRep,
                          returnVariable: InternalVariableRep,
                          activations: List[InternalActivationRep])

  private val factFormat = new ObjectResultFormat(Set(
    NamedPropertyFormat("uid", formats.NumberFormat, "Unique id for this fact inside this method"),
    NamedPropertyFormat("identifier", formats.StringFormat, "Unique string representation of this fact"),
    NamedPropertyFormat("displayName", formats.StringFormat, "Display name for this fact")
  ))

  case class FactRep(uid: Int, identifier: String, displayName: String)

  private val methodFormat = new ObjectResultFormat(Set(
    NamedPropertyFormat("name", formats.StringFormat, "Name of this method"),
    NamedPropertyFormat("declaringClassName", formats.StringFormat, "Name of the class this method is defined in"),
    NamedPropertyFormat("descriptor", formats.StringFormat, "String representation of this method's descriptor"),
    NamedPropertyFormat("statements", ListResultFormat(stmtFormat, "Objects representing individual statements of the method"), "List containing all method statements"),
    NamedPropertyFormat("facts", ListResultFormat(factFormat, "Object representing individual facts occurring in this method"), "List of all facts relevant for this method")
  ))

  case class MethodIFDSRep(name: String, declaringClassName: String, descriptor: String, statements: List[StatementRep], facts: List[FactRep])

  def buildDescriptor(aName: String, aVersion: String, aDescription: String): AnalysisImplementationDescriptor = new AnalysisImplementationDescriptor {

    override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Method

    override val analysisData: AnalysisData = AnalysisData.systemAnalysis(
      aName,
      aVersion,
      aDescription,
      "OPAL 4.0.0",
      Set("java", "scala"),
      methodFormat,
      SoftwareEntityKind.Program,
      doesBatchProcessing = true,
      isIncremental = true
    )
  }
}

trait DefaultIFDSMethodRepJsonFormat extends DefaultJsonProtocol {

  implicit val internalVariableRepFormat: JsonFormat[InternalVariableRep] = jsonFormat2(InternalVariableRep)
  implicit val internalActivationRepFormat: JsonFormat[InternalActivationRep] = jsonFormat2(InternalActivationRep)
  implicit val statementRepFormat: JsonFormat[StatementRep] = jsonFormat11(StatementRep)
  implicit val factRepFormat: JsonFormat[FactRep] = jsonFormat3(FactRep)
  implicit val methodIFDSRep: JsonFormat[MethodIFDSRep] = jsonFormat5(MethodIFDSRep)

}
