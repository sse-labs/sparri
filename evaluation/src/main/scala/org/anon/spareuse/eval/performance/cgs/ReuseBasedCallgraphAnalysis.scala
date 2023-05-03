package org.anon.spareuse.eval.performance.cgs

import org.anon.spareuse.core.model.entities.JavaEntities.JavaClass
import org.anon.spareuse.core.utils.http.HttpDownloadException
import org.anon.spareuse.eval.{gavToEntityId, getAllTypesForProgram, getAnalysisResultsForEntity, runFinished, timedExec, triggerAnalysisRun}
import org.apache.http.impl.client.HttpClients
import spray.json.{JsArray, JsNumber, JsObject, JsString}

import scala.annotation.switch
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class ReuseBasedCallgraphAnalysis(apiBaseUrl: String) extends WholeProgramCgAnalysis {

  private val httpClient = HttpClients.createDefault()
  private val analysisName = "mvn-partial-callgraphs"
  private val analysisVersion = "1.0.0"

  private var theCg: Option[StitchedCallGraph] = None

  override def prepareData(rootGav: String, dependencyGavs: Set[String]): Try[Unit] = Try {
    val timedOp = timedExec { () =>
      theCg = Some(new StitchedCallGraph(rootGav, dependencyGavs))

      def toMethodLookup(json: JsArray, gav: String): Map[Long, MethodRepr] = {
        json.elements.map {
          case jo: JsObject =>
            val mId = jo.fields("mId").asInstanceOf[JsNumber].value.longValue()
            val mFqn = jo.fields("mFqn").asInstanceOf[JsString].value
            val reachable = jo.fields("reachable").asInstanceOf[JsNumber].value > 0

            val callSites = jo.fields("css").asInstanceOf[JsArray].elements.map {
              case siteObj: JsObject =>
                val pc = siteObj.fields("csPc").asInstanceOf[JsNumber].value.intValue()
                val isIncomplete = siteObj.fields("incomplete").asInstanceOf[JsNumber].value.intValue() > 0
                val instrRep = siteObj.fields("instr").asInstanceOf[JsString].value
                val targets = siteObj.fields("targets").asInstanceOf[JsArray].elements.map {
                  case n: JsNumber => n.value.longValue()
                  case _ => throw new IllegalStateException("Invalid format of partial results")
                }.toList

                CallSiteRepr(pc, isIncomplete, targets, instrRep)
              case _ =>
                throw new IllegalStateException("Invalid format of partial results")
            }.toList

            (mId, MethodRepr(mId, gav, mFqn, reachable, callSites))
          case _ =>
            throw new IllegalStateException("Invalid format of partial results")
        }.toMap

      }

      def toTypeHierarchy(json: JsArray): Seq[DeclaredTypeNode] = {
        json.elements.map {
          case jo: JsObject =>
            val tId = jo.fields("id").asInstanceOf[JsNumber].value.longValue()
            val tFqn = jo.fields("fqn").asInstanceOf[JsString].value
            val isInterface = jo.fields("interface").asInstanceOf[JsNumber].value.intValue() > 0
            val superType = jo.fields("superId").asInstanceOf[JsNumber].value.longValue()
            val interfaces = jo.fields("interfaceIds").asInstanceOf[JsArray].elements.map {
              case n: JsNumber => n.value.longValue()
              case _ => throw new IllegalStateException("Invalid format of partial results")
            }

            DeclaredTypeNode(tId, tFqn, isInterface, if(superType == -1) None else Some(superType), interfaces)
        }
      }




      getAnalysisResultsForEntity(gavToEntityId(rootGav), analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
        case jo: JsObject =>
          val methodInfo = jo.fields.get("graph").map(_.asInstanceOf[JsArray]).getOrElse(throw new IllegalStateException("Expected a JSON Array with method info"))
          val typesInfo = jo.fields.get("types").map(_.asInstanceOf[JsArray]).getOrElse(throw new IllegalStateException("Expected a JSON Array with type info"))
          val cg = PartialCallGraph(rootGav, toMethodLookup(methodInfo, rootGav))
          theCg.get.addPartialInfo(rootGav, cg, toTypeHierarchy(typesInfo))
        case _ =>
          throw new IllegalStateException("Expected a JSON Array")
      }

      dependencyGavs
        .foreach { gav =>
          val entityId = gavToEntityId(gav)
          getAnalysisResultsForEntity(entityId, analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
            case jo: JsObject =>
              val methodInfo = jo.fields.get ("graph").map (_.asInstanceOf[JsArray] ).getOrElse (throw new IllegalStateException ("Expected a JSON Array with method info") )
              val typesInfo = jo.fields.get ("types").map (_.asInstanceOf[JsArray] ).getOrElse (throw new IllegalStateException ("Expected a JSON Array with type info"))

              val cg = PartialCallGraph(gav, toMethodLookup(methodInfo, gav))
              theCg.get.addPartialInfo(gav, cg, toTypeHierarchy(typesInfo))
            case _ =>
              throw new IllegalStateException("Expected a JSON Array")
          }
        }

      logger.info(s"Successfully retrieved ${dependencyGavs.size + 1} partial callgraphs")
    }

    logger.info(s"Preparing data took ${timedOp.getDurationMillis}ms")

    timedOp.getContent
  }

  override def buildFullCallgraph(): Try[StitchedCallGraph] = Try {
    val cg = theCg.get

    cg.resolveAll()

    logger.info(s"Got a stitched callgraph with ${cg.reachableMethods().size} reachable methods and ${cg.numEdges()} edges")

    cg
  }


  def ensureAllPartialResultsPresent(allGavs: Set[String]): Try[Unit] = Try {

    val expectedIds = allGavs.map(gavToEntityId)

    def startNewRunAndAwaitFinished(): Unit = {
      triggerAnalysisRun(allGavs.map(gavToEntityId), analysisName, analysisVersion, apiBaseUrl, httpClient, configuration = "--algorithm rta") match {
        case Success(runLocation) =>
          logger.info(s"Successfully triggered analysis run. Waiting for run at $runLocation to complete...")

          while (!runFinished(runLocation, apiBaseUrl, httpClient).get) {
            Thread.sleep(1000)
            logger.debug(s"Waiting for run to finish: $runLocation ...")
          }

          logger.info("All partial callgraphs are available.")
        case Failure(ex) =>
          logger.error("Failed to trigger analysis run for partial callgraphs", ex)
      }
    }

    // Check whether all required entities have the required results. They may be spread across different analysis runs!
    val responses = expectedIds
      .map(getAnalysisResultsForEntity(_, analysisName, analysisVersion, apiBaseUrl, httpClient))

    val apiIsMissingResults = responses
      .collect {
        case Failure(HttpDownloadException(404, url, _)) => url
      }
      .nonEmpty
    // If at least one result is missing: Start a new analysis run for all inputs (existing will be filtered out)
    if (apiIsMissingResults) startNewRunAndAwaitFinished()


  }

  def close(): Unit = httpClient.close()

  def cleanup(): Unit = {
    theCg = None
  }


  case class PartialCallGraph(gav: String, methodLookup: Map[Long, MethodRepr])

  case class MethodRepr(id: Long, graphGav: String, fqn: String, reachable: Boolean, callSites: List[CallSiteRepr]){

    lazy val identifier: MethodIdentifier = toIdentifier(fqn)

    lazy val incompleteCallSites: Seq[CallSiteRepr] = callSites.filter(_.isIncomplete)

    lazy val hasIncompleteCallSites: Boolean = incompleteCallSites.nonEmpty

    val isReachable: Boolean = reachable

    private def toIdentifier(methodIdent: String): MethodIdentifier = {

      var state = 0
      val builder = new mutable.StringBuilder()

      var declaringType: String = null
      var visibility: String = ""
      var isStatic: Boolean = false
      var returnType: String = null
      var name: String = null
      val arguments = new ListBuffer[String]()

      for(c <- methodIdent.toCharArray){
        (state: @switch) match {
          case 0 =>
            if(c == '{'){
              declaringType = builder.toString()
              builder.clear()
              state = 2
            } else if(!c.isSpaceChar) builder.append(c)

          case 2 =>
            if(c.isSpaceChar && builder.nonEmpty){
              val tmp = builder.toString()
              if(tmp.equalsIgnoreCase("static")){
                isStatic = true
                builder.clear() // Stay in this state !
              } else if(tmp.equalsIgnoreCase("public") || tmp.equalsIgnoreCase("private") || tmp.equalsIgnoreCase("protected")){
                visibility = tmp
                builder.clear() // Stay in this state !
              } else {
                returnType = tmp
                builder.clear()
                state = 3
              }
            } else if(!c.isSpaceChar) builder.append(c)

          case 3 =>
            if(c == '('){
              name = builder.toString()
              builder.clear()
              state = 4
            } else if(!c.isSpaceChar) builder.append(c)

          case 4 =>
            if(c == ')'){
              if(builder.nonEmpty) arguments.append(builder.toString())
              builder.clear()
              return MethodIdentifier(declaringType, name, returnType, arguments, isStatic, visibility)
            } else if(c == ',') {
              arguments.append(builder.toString())
              builder.clear()
            } else if (!c.isSpaceChar) builder.append(c)

          case _ =>
        }
      }

      throw new IllegalStateException(s"Failed to parse method identifier $methodIdent")
    }

  }

  case class MethodIdentifier(mDeclaringTypeFqn: String, mName: String, mRetType: String, mArgumentTypes: Seq[String], isStatic: Boolean, visibility: String)

  case class CallSiteRepr(pc: Int, isIncomplete: Boolean, targets: List[Long], instrRep: String) {

    lazy val instruction: InvocationInstr = parseInstruction(instrRep)


    private def parseInstruction(instrRep: String): InvocationInstr = {
      val arr = instrRep.toCharArray

      var state = 0

      val builder = new mutable.StringBuilder

      var kindStr: String = null
      var typeStr: String = null
      var retStr: String = null
      var nameStr: String = null
      val arguments = new ListBuffer[String]

      for (c <- arr) {
        (state: @switch) match {
          case 0 =>
            if (c == '/') state = 1
            else if (c == '(') {
              kindStr = builder.toString()
              builder.clear()
              state = 4
            }
            else if (!c.isSpaceChar) builder.append(c)

          case 1 =>
            if (c == '*') state = 2

          case 2 =>
            if (c == '*') state = 3

          case 3 =>
            if (c == '/') state = 0

          case 4 =>
            if (c == '{') {
              typeStr = builder.toString()
              builder.clear()
              state = 5
            } else if (!c.isSpaceChar) builder.append(c)

          case 5 =>
            if (c.isSpaceChar && builder.nonEmpty) {
              retStr = builder.toString()
              builder.clear()
              state = 6
            } else if (!c.isSpaceChar) builder.append(c)

          case 6 =>
            if (c == '(') {
              nameStr = builder.toString()
              builder.clear()
              state = 7
            } else if (!c.isSpaceChar) builder.append(c)

          case 7 =>
            if (c == ')') {
              if (builder.nonEmpty) arguments.append(builder.toString())
              builder.clear()
              return InvocationInstr(kindStr, typeStr, nameStr, retStr, arguments.toList)
            } else if (c == ',') {
              arguments.append(builder.toString())
              builder.clear()
            } else if (!c.isSpaceChar) builder.append(c)

          case _ =>
        }
      }
      InvocationInstr(kindStr, typeStr, nameStr, retStr, arguments.toList)
    }

  }

  case class InvocationInstr(invocationType: String,
                             declaredTypeFqn: String,
                             methodName: String,
                             returnType: String,
                             params: Seq[String]){
    lazy val isStatic: Boolean = invocationType.equalsIgnoreCase("INVOKESTATIC")
    lazy val isVirtual: Boolean = invocationType.equalsIgnoreCase("INVOKEVIRTUAL")
    lazy val isSpecial: Boolean = invocationType.equalsIgnoreCase("INVOKESPECIAL")
    lazy val isInterface: Boolean = invocationType.equalsIgnoreCase("INVOKEINTERFACE")
  }

  case class DeclaredTypeNode(id: Long, fqn: String, isInterface: Boolean, superTypeId: Option[Long], interfaceIds: Seq[Long])
  case class ResolvedDeclaredType(id: Long, fqn: String, isInterface: Boolean, superTypeName: Option[String], interfaceTypeNames: Seq[String])
  class TypeNode(fqn: String, decl: ResolvedDeclaredType, methods: Set[MethodRepr], instantiated: Boolean, subNodes: Set[TypeNode] = Set.empty, superNode: Option[TypeNode] = None, interfaces: Set[TypeNode] = Set.empty){

    val typeFqn: String = fqn
    val typeDeclaration: ResolvedDeclaredType = decl
    val allMethods: Set[MethodRepr] = methods
    val isInstantiated: Boolean = instantiated

    lazy val methodLookup: Map[String, Set[MethodRepr]] =
      methods
        .map(_.identifier.mName)
        .map { name =>
          (name, methods.filter(m => m.identifier.mName.equalsIgnoreCase(name)))
        }
        .toMap


    private val children = mutable.Set[TypeNode](subNodes.toList :_*)
    private var parent = superNode
    private var parentInterfaces = interfaces

    def superType: Option[TypeNode] = parent
    def interfaceTypes: Set[TypeNode]= parentInterfaces
    def subTypes: Set[TypeNode] = children.toSet

    def addSubType(child: TypeNode): Unit = children.add(child)
    def setSuperType(p: TypeNode): Unit = {
      parent = Some(p)
      p.addSubType(this)
    }

    def setInterfaceTypes(i: Set[TypeNode]): Unit = {
      parentInterfaces = i
      parentInterfaces.foreach{ _.addSubType(this) }
    }

    def allSubTypes: Set[TypeNode] = {
      subTypes ++ subTypes.flatMap(_.allSubTypes)
    }

    def allParents: Set[TypeNode] = {
      val directParents = if(parent.isDefined) Set(parent.get) ++ parentInterfaces
        else parentInterfaces

      directParents ++ directParents.flatMap(_.allParents)
    }
  }


  class StitchedCallGraph(rootGav: String, dependencyGavs: Set[String]){

    private val partialCgs = new mutable.HashMap[String, PartialCallGraph]()

    private val rawTypesLookup = new mutable.HashMap[String, ResolvedDeclaredType]()
    private val rawMethodByTypeLookup = new mutable.HashMap[String, mutable.Set[MethodRepr]]

    private var typesLookup = Map.empty[String, TypeNode]

    private var allReachableMethods: Set[MethodRepr] = Set.empty
    private var allCallees: Map[MethodRepr, Map[Int, Set[MethodRepr]]] = Map.empty

    def reachableMethods(): Set[MethodRepr] = allReachableMethods

    def callSitesOf(m: MethodRepr): Map[Int, Set[MethodRepr]] = allCallees(m)
    def allCalleesOf(m: MethodRepr): Set[MethodRepr] = allCallees(m).flatMap(_._2).toSet

    def numEdges(): Int = allCallees.values.flatMap(v => v.values.map(_.size)).sum

    def addPartialInfo(gav: String, partialCallGraph: PartialCallGraph, types: Seq[DeclaredTypeNode]): Unit = {
      partialCgs.put(gav, partialCallGraph)
      partialCallGraph.methodLookup.values.foreach(r => {
        val tFqn = r.identifier.mDeclaringTypeFqn
        if(!rawMethodByTypeLookup.contains(tFqn)) rawMethodByTypeLookup.put(tFqn, mutable.Set.empty)
        val currMethods = rawMethodByTypeLookup(tFqn)
        if(!currMethods.exists(m => m.fqn.equals(r.fqn)))
          rawMethodByTypeLookup(tFqn).add(r)
      })

      val partialTypesLookup = types.map(t => (t.id, t)).toMap

      types.foreach{ t =>
        val superTypeOpt = t.superTypeId.map(partialTypesLookup.get(_).map(_.fqn).getOrElse("java.lang.Object"))
        val interfaceTypes = t.interfaceIds.flatMap(i => if(partialTypesLookup.contains(i)) Some(partialTypesLookup(i).fqn) else None)
        val resolvedType = ResolvedDeclaredType(t.id, t.fqn, t.isInterface, superTypeOpt, interfaceTypes)

        rawTypesLookup.put(t.fqn.replace("/", "."), resolvedType)
      }
    }

    def resolveAll(): Unit = {

      // Collect all instantiated types, this will parse all instructions!
      val timeInstantiatedTypesOp = timedExec { () =>
        rawMethodByTypeLookup
          .values
          .flatten
          .flatMap(_.callSites)
          .filter(_.instruction.isSpecial)
          .map(_.instruction.declaredTypeFqn)
          .toSet
      }

      val instantiatedTypes = timeInstantiatedTypesOp.getContent

      logger.info(s"Collecting instantiated types took ${timeInstantiatedTypesOp.getDurationMillis}ms")

      // Build full type hierarchy
      val timeHierarchyOp = timedExec { () =>
        typesLookup = rawTypesLookup.keys.map{ fqn =>
          val methods = rawMethodByTypeLookup.get(fqn).map(_.toSet).getOrElse(Set.empty)
          val classRepr = rawTypesLookup(fqn)
          val isInstantiated = instantiatedTypes.contains(fqn)
          (fqn, new TypeNode(fqn, classRepr, methods, isInstantiated))
        }.toMap
        typesLookup.values.foreach { typeNode =>
          typeNode.typeDeclaration.superTypeName.flatMap(typesLookup.get(_)).foreach(p => typeNode.setSuperType(p))

          typeNode.setInterfaceTypes(typeNode.typeDeclaration.interfaceTypeNames.map(_.replace("/", ".")).filter(typesLookup.contains).map(typesLookup(_)).toSet)
        }
      }

      logger.info(s"Building type hierarchy in ${timeHierarchyOp.getDurationMillis}ms")

      var start = System.currentTimeMillis()
      val reachableMethods: mutable.Set[MethodRepr] = mutable.Set(partialCgs(rootGav).methodLookup.values.filter(_.isReachable).toSeq :_* )

      val calleesLookup: mutable.Map[MethodRepr, Map[Int, mutable.Set[MethodRepr]]] = mutable.Map(
        reachableMethods.map { m =>
          val targetSet = m.callSites.map{ callSite =>
            (callSite.pc, mutable.Set(callSite.targets.map( mId => partialCgs(rootGav).methodLookup(mId)) :_*))
          }.toMap
          (m, targetSet)
        }.toSeq :_*
      )
      val toResolve: mutable.Set[MethodRepr] = mutable.Set(reachableMethods.filter(_.hasIncompleteCallSites).toSeq :_*)

      val t1 = System.currentTimeMillis() - start
      start = System.currentTimeMillis()

      logger.info(s"Preparing data structures took $t1 ms")

      // MAIN RESOLVER LOOP
      while(toResolve.nonEmpty){
        val currentMethod = toResolve.head
        toResolve.remove(currentMethod)

        val currentCallees = calleesLookup(currentMethod)

        // Process all incomplete Callsites for a given method
        currentMethod.incompleteCallSites.foreach { callSite =>

          if(!callSite.instruction.declaredTypeFqn.startsWith("java.") ){ //Not resolving calls to JRE in both analyses
            // Get (mutable) Set of targets so far
            val currentTargets = currentCallees(callSite.pc)

            if (callSite.instruction.isStatic || callSite.instruction.isSpecial) {

              val logStr = if(callSite.instruction.isStatic) "static" else "special"

              // For static or special callsites we just need to locate the declared target type
              val typeFqn = callSite.instruction.declaredTypeFqn
              if (!typesLookup.contains(typeFqn)) {
                // This is bad, seem to be missing the type for the static invocation
                logger.warn(s"Unknown type for $logStr invocation: $typeFqn")
              } else {

                // Get the type node and find the one declared method that has a) the same name and b) the same parameters
                val typeNode = typesLookup(typeFqn)
                val targets = typeNode
                  .methodLookup
                  .getOrElse(callSite.instruction.methodName, Set.empty)
                  .filter(m => m.identifier.mArgumentTypes.equals(callSite.instruction.params))

                // There should really only be precisely one matching method for static invocations
                if (targets.isEmpty) logger.warn(s"No matching methods for $logStr invocation ${callSite.instrRep}")
                else if (targets.size > 1) logger.warn(s"Too may matching methods (${targets.size}) for $logStr invocation ${callSite.instrRep}")
                else {
                  val theTarget = targets.head

                  // Add this new target method to the set
                  calleesLookup(currentMethod)(callSite.pc).add(theTarget)

                  // Add new target method to set of reachable methods. If it was not present before (i.e. 'new'), add it to worklist as well
                  if (reachableMethods.add(theTarget)) {
                    toResolve.add(theTarget)
                  }

                  // Make sure the new target method is contained in the callee lookup
                  if (!calleesLookup.contains(theTarget)) {
                    val targetGraphMethods = partialCgs(theTarget.graphGav).methodLookup

                    calleesLookup.put(theTarget, theTarget
                      .callSites
                      .map(cs => (cs.pc, mutable.Set(cs.targets.map(tId => targetGraphMethods(tId)): _*)))
                      .toMap
                    )

                  }
                }
              }
            } else {

              val allTypesCHA = {
                val targetTypeFqn = callSite.instruction.declaredTypeFqn
                if (!typesLookup.contains(targetTypeFqn)) {
                  //logger.warn(s"Unknown declared type for non-static invocation: $targetTypeFqn")
                  typesLookup.values.toSet //TODO: Deal with types we don't know (i.e. JRE types)
                } else {
                  val targetType = typesLookup(targetTypeFqn)
                  targetType.allSubTypes ++ Set(targetType)
                }
              }
              var methods = allTypesCHA
                .filter(_.isInstantiated) // This is RTA :-)
                .flatMap { t =>
                  t
                    .methodLookup
                    .getOrElse(callSite.instruction.methodName, Set.empty)
                    .filter(m => m.identifier.mArgumentTypes.equals(callSite.instruction.params))
                }


              if(methods.isEmpty && typesLookup.contains(callSite.instruction.declaredTypeFqn)){
                methods = typesLookup(callSite.instruction.declaredTypeFqn)
                  .allParents
                  .flatMap { t =>
                    t
                      .methodLookup
                      .getOrElse(callSite.instruction.methodName, Set.empty)
                      .filter(m => m.identifier.mArgumentTypes.equals(callSite.instruction.params))
                  }
              }

              methods.foreach { m =>
                if (!currentTargets.contains(m)) {
                  currentTargets.add(m)
                }

                if (!calleesLookup.contains(m)) {
                  val targetMethods = partialCgs(m.graphGav).methodLookup

                  calleesLookup.put(m, m
                    .callSites
                    .map(cs => (cs.pc, mutable.Set(cs.targets.map(tId => targetMethods(tId)): _*)))
                    .toMap
                  )

                }

                if (reachableMethods.add(m)) {
                  toResolve.add(m)
                }
              }
            }
          }
        }
      }

      val t2 = System.currentTimeMillis() - start
      logger.info(s"Resolver loop took $t2 ms and found ${reachableMethods.size} reachable Methods")

      allReachableMethods = reachableMethods.toSet
      allCallees = calleesLookup.mapValues(v => v.mapValues(_.toSet)).toMap
    }






  }
}
