package org.anon.spareuse.execution.analyses.impl

import org.anon.spareuse.core.formats
import org.anon.spareuse.core.formats.{AnalysisResultFormat, ListResultFormat, NamedPropertyFormat, ObjectResultFormat}
import org.anon.spareuse.core.maven.MavenIdentifier
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.JavaEntities.JavaProgram
import org.anon.spareuse.core.model.{AnalysisData, SoftwareEntityKind}
import org.anon.spareuse.core.model.entities.SoftwareEntityData
import org.anon.spareuse.core.opal.OPALProjectHelper
import org.anon.spareuse.core.utils.http.HttpDownloadException
import MvnPartialCallgraphAnalysisImpl.{InternalCallSite, InternalMethod, RTAResult, TypeNode, parseConfig, validCallgraphAlgorithms}
import org.anon.spareuse.execution.analyses.{AnalysisImplementation, AnalysisImplementationDescriptor, AnalysisResult, FreshResult}
import org.opalj.br.instructions.Instruction
import org.opalj.br.{DeclaredMethod, Method, ObjectType}
import org.opalj.tac.cg.{CHACallGraphKey, CTACallGraphKey, RTACallGraphKey, XTACallGraphKey}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class MvnPartialCallgraphAnalysisImpl extends AnalysisImplementation {

  override val descriptor: AnalysisImplementationDescriptor = MvnPartialCallgraphAnalysisImpl

  override def executionPossible(inputs: Seq[SoftwareEntityData], rawConfig: String): Boolean = {

    if(rawConfig.trim.isBlank) return true

    val parts = rawConfig.trim.split(" ")

    var i = 0

    while(i < parts.length){
      if(parts(i).toLowerCase.equals("--algorithm")){
        if( i >= parts.length - 1 || !validCallgraphAlgorithms.exists( algo => algo.toLowerCase.equals(parts(i + 1)))) return false
        else i += 1 // Skip next entry, already was verified!
      } else if(!parts(i).toLowerCase.equals("--use-jre") && !parts(i).equals("--application-mode")) return false

      i += 1
    }

    inputs.forall( sed => sed.isInstanceOf[JavaProgram])
  }

  override def executeAnalysis(inputs: Seq[SoftwareEntityData], rawConfig: String): Try[Set[AnalysisResult]] = Try {

    val opalHelper = new OPALProjectHelper(loadJreClassImplementation = false)
    val config = parseConfig(rawConfig)

    val opalCgKey = config.algorithm match {
      case "cha" => CHACallGraphKey
      case "rta" => RTACallGraphKey
      case "xta" => XTACallGraphKey
      case "cta" => CTACallGraphKey
      case a@_ =>
        log.warn(s"Invalid CG key after validation: $a")
        XTACallGraphKey
    }


    inputs.map( sed => sed.asInstanceOf[JavaProgram] ).flatMap { program =>
      getFileFor(program) match {
        case Success(inputStream) =>
          val jarUrl = MavenIdentifier.fromGAV(program.name).map(_.toJarLocation.toURL).get
          val classes = opalHelper.readClassesFromJarStream(inputStream, jarUrl, loadImplementation = true).get
          val project = opalHelper.buildOPALProject(classes, List.empty, config.includeJre, setLibraryMode = !config.applicationMode)


          val cg = project.get(opalCgKey)
          val methodIdCnt = new AtomicInteger(0)


          // Build Internal representations without callsites (to assign ids)
          def toMethodObj(definedMethod: Method, reachable: Boolean = true): InternalMethod = {
            new InternalMethod(methodIdCnt.getAndIncrement(), definedMethod.toJava, reachable, List.empty)
          }

          val methodObjLookup: Map[DeclaredMethod, Seq[InternalMethod]] = cg
            .reachableMethods()
            .map(c => c.method)
            .filter{ declMethod => // Filter out non-project (i.e. JRE) methods
              project.isProjectType(declMethod.declaringClassType)
            }
            .map { declMethod =>
              if(declMethod.hasMultipleDefinedMethods){
                (declMethod, declMethod
                  .asMultipleDefinedMethods
                  .definedMethods
                  .map(m => toMethodObj(m)))
              } else if(declMethod.hasSingleDefinedMethod){
                (declMethod, Seq(toMethodObj(declMethod.asDefinedMethod.definedMethod)))
              } else {
                (declMethod, Seq(new InternalMethod(methodIdCnt.getAndIncrement(), declMethod.toJava, isReachable = true, List.empty)))
              }
            }.toMap

          val allMethodDataWithCallSites = cg
            .reachableMethods()
            .map(c => c.method)
            .filter { declMethod => // Filter out non-project (i.e. JRE) methods
              project.isProjectType(declMethod.declaringClassType)
            }
            .flatMap{ declMethod =>
              val incompleteCsPcs = cg.incompleteCallSitesOf(declMethod).toSet

              val methodBodies = if(declMethod.hasMultipleDefinedMethods) declMethod.asMultipleDefinedMethods.definedMethods.map(_.body)
                else if(declMethod.hasSingleDefinedMethod) Seq(declMethod.asDefinedMethod.definedMethod.body)
                else Seq.empty

              def getCallSiteInstruction(pc: Int): Option[Instruction] = {
                methodBodies
                  .find(_.isDefined)
                  .map{ codeOpt =>
                    codeOpt.get.instructions(pc)
                  }
              }

              val allMethodCallSites = cg
                .calleesOf(declMethod)
                .map{ callSiteInfo =>
                  val pc = callSiteInfo._1
                  new InternalCallSite(
                    pc, // PC of this callsite
                    callSiteInfo._2.flatMap(c => methodObjLookup.getOrElse(c.method, Seq.empty).map(_.mId)).toList, // All ids of confirmed targets of this callsite
                    incompleteCsPcs.contains(pc), // Whether this callsite is incomplete atm
                    getCallSiteInstruction(pc).map(_.toString(pc)).getOrElse("<none>")
                  )
                }
                .toList

              methodObjLookup(declMethod).map(mObj => mObj.withCallSiteInfo(allMethodCallSites))
            }
            .toList

          val reachableMethodSignatures = cg.reachableMethods().map(_.method).filter(m => project.isProjectType(m.declaringClassType)).map(_.toJava).toSet
          val extraMethods = project
            .allMethods
            .filterNot( method => reachableMethodSignatures.contains(method.toJava))
            .map { method =>
              val id = methodIdCnt.getAndIncrement()
              val fqn = method.toJava

              val callsites = method.body.map{ body =>
                val instrs = new ListBuffer[InternalCallSite]()
                for(pc <- body.programCounters){
                  val instr = body.instructions(pc)
                  if(instr != null && instr.isInvocationInstruction){
                    new InternalCallSite(pc, List.empty, true, instr.toString(pc))
                  }
                }
                instrs.toList
              }.getOrElse(List.empty)

              new InternalMethod(id, fqn, isReachable = false, callsites)
            }



          def toTypeNode(t: ObjectType): TypeNode = {
            val classFile = project.classFile(t)

            val superId = classFile.flatMap(_.superclassType).map(_.id.toLong)
            val interfaceTypes = classFile.map(_.interfaceTypes.map(_.id.toLong)).getOrElse(Seq.empty[Long])

            new TypeNode(t.id, t.fqn, project.classFile(t).exists(_.isInterfaceDeclaration), superId, interfaceTypes)
          }

          val types = project.allProjectClassFiles.map(cf => toTypeNode(cf.thisType)).toList

          val resultContent = new RTAResult(allMethodDataWithCallSites ++ extraMethods, types)

          log.info(s"Done building partial callgraph representation for ${program.name}")

          Some(FreshResult(resultContent, Set(program)))

        case Failure(HttpDownloadException(status, _, _)) if status == 404 =>
          log.warn(s"No JAR file available for ${program.identifier}")
          None
        case Failure(ex) =>
          log.error(s"Failed to download JAR file contents for ${program.identifier}", ex)
          throw ex

      }
    }.toSet

  }
}

object MvnPartialCallgraphAnalysisImpl extends AnalysisImplementationDescriptor {

  val validCallgraphAlgorithms: Set[String] = Set("cha", "rta", "cta", "xta")

  private val resultFormat: AnalysisResultFormat = ObjectResultFormat(
    NamedPropertyFormat("graph",
      ListResultFormat(
        ObjectResultFormat(Set(
          NamedPropertyFormat("mId", formats.NumberFormat, "The unique id assigned to this method"),
          NamedPropertyFormat("mFqn", formats.StringFormat, "The fully qualified name of this method"),
          NamedPropertyFormat("reachable", formats.NumberFormat, "Indicates whether this method was reachable in the partial callgraph"),
          NamedPropertyFormat("css", ListResultFormat(ObjectResultFormat(Set(
            NamedPropertyFormat("csPc", formats.NumberFormat, "PC of this callsite inside the enclosing method body"),
            NamedPropertyFormat("targets", ListResultFormat(formats.NumberFormat), "List of method ids that have been resolved as targets for this callsite"),
            NamedPropertyFormat("incomplete", formats.NumberFormat, "Indicates whether the resolution of this callsite was incomplete"),
            NamedPropertyFormat("instr", formats.StringFormat, "String representation of the callsite instruction")
          )), "List of callsites for this method"))
        ))
        , "List of Method definitions with callsite information")
      , "The actual RTA callgraph"),
    NamedPropertyFormat("types",
      ListResultFormat(ObjectResultFormat(
        NamedPropertyFormat("id", formats.NumberFormat, "The unique id for this type"),
        NamedPropertyFormat("fqn", formats.StringFormat, "The type FQN"),
        NamedPropertyFormat("interface", formats.NumberFormat, "Number indicating whether this type is an interface definition"),
        NamedPropertyFormat("superId", formats.NumberFormat, "The id of the super type, or -1 if there is none"),
        NamedPropertyFormat("interfaceIds", formats.ListResultFormat(formats.NumberFormat), "The set of interface type ids")
      ))
      , "A list of type nodes")
  )

  private val resultFormat2: AnalysisResultFormat = ListResultFormat(
    ObjectResultFormat(Set(
      NamedPropertyFormat("mId", formats.NumberFormat, "The unique id assigned to this method"),
      NamedPropertyFormat("mFqn", formats.StringFormat, "The fully qualified name of this method"),
      NamedPropertyFormat("reachable", formats.NumberFormat, "Indicates whether this method was reachable in the partial callgraph"),
      NamedPropertyFormat("css", ListResultFormat(ObjectResultFormat(Set(
        NamedPropertyFormat("csPc", formats.NumberFormat, "PC of this callsite inside the enclosing method body"),
        NamedPropertyFormat("targets", ListResultFormat(formats.NumberFormat), "List of method ids that have been resolved as targets for this callsite"),
        NamedPropertyFormat("incomplete", formats.NumberFormat, "Indicates whether the resolution of this callsite was incomplete"),
        NamedPropertyFormat("instr", formats.StringFormat, "String representation of the callsite instruction")
      )), "List of callsites for this method"))
    ))
    , "List of Method definitions with callsite information")

  override val analysisData: AnalysisData = AnalysisData.systemAnalysis(
    "mvn-partial-callgraphs",
    "1.0.0",
    "This analysis uses OPAL to calculate a partial callgraph for a given Maven program (GAV-Triple). The graph is returned as a list of methods with unique ids and references to callees. " +
      "Multiple construction algorithms are supported (use '--algorithm cha|rta|xta|cta'), default is RTA. Use '--use-jre' to include JRE implementations in callgraph.",
    "OPAL",
    Set("java", "scala"),
    resultFormat,
    SoftwareEntityKind.Program,
    doesBatchProcessing = true,
    isIncremental = false)


  override val requiredInputResolutionLevel: SoftwareEntityKind = SoftwareEntityKind.Package

  case class PartialCallgraphAnalysisConfig(algorithm: String, includeJre: Boolean, applicationMode: Boolean)

  def parseConfig(raw: String): PartialCallgraphAnalysisConfig = {
    var algo = "xta"
    var includeJre = false
    var appMode = false

    val parts = raw.trim.split(" ")

    for (i <- Range(0, parts.length)) {
      if (parts(i).toLowerCase.equals("--algorithm")) {
        algo = parts(i + 1)
      } else if (parts(i).toLowerCase.equals("--use-jre") ) {
        includeJre = true
      } else if(parts(i).toLowerCase.equals("--application-mode")){
        appMode = true
      }
    }

    PartialCallgraphAnalysisConfig(algo, includeJre, appMode)
  }

  class InternalMethod(id: Int, fqn: String, isReachable: Boolean, callSites: List[InternalCallSite]){

    def withCallSiteInfo(info: List[InternalCallSite]) =
      new InternalMethod(id, fqn, isReachable, info)

    val mId: Int = id
    val mFqn: String = fqn
    val reachable: Int = if(isReachable) 1 else 0
    val css: List[InternalCallSite] = callSites
  }

  class InternalCallSite(pc: Int, resolvedTargetIds: List[Int], isIncomplete: Boolean, instruction: String) {

    val csPc: Int = pc
    val targets: List[Int] = resolvedTargetIds
    val incomplete: Int = if(isIncomplete) 1 else 0
    val instr: String = instruction
  }

  class TypeNode(tid: Long, typeFqn: String, isInterface: Boolean, superType: Option[Long], interfaces: Seq[Long]) {

    val id: Long = tid
    val fqn: String = typeFqn
    val interface: Long = if (isInterface) 1L else 0L
    val superId: Long = superType.getOrElse(-1L)
    val interfaceIds: Seq[Long] = interfaces
  }

  class RTAResult(graphInfo: List[InternalMethod], typeInfo: List[TypeNode]){

    val graph: List[InternalMethod] = graphInfo
    val types: List[TypeNode] = typeInfo
  }

}
