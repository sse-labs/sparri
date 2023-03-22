package de.tudo.sse.spareuse.eval.performance.cgs
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaClass
import de.tudo.sse.spareuse.eval.performance.{gavToEntityId, getAllTypesForProgram, getAnalysisResultsForEntity, runFinished, timedExec, triggerAnalysisRun}
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

            (mId, MethodRepr(mId, gav, mFqn, callSites))
          case _ =>
            throw new IllegalStateException("Invalid format of partial results")
        }.toMap

      }


      getAnalysisResultsForEntity(gavToEntityId(rootGav), analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
        case ja: JsArray =>
          val types = getAllTypesForProgram(rootGav, apiBaseUrl, httpClient).get
          val cg = PartialCallGraph(rootGav, toMethodLookup(ja, rootGav))
          theCg.get.addPartialInfo(rootGav, cg, types)
        case _ =>
          throw new IllegalStateException("Expected a JSON Array")
      }

      dependencyGavs
        .foreach { gav =>
          val entityId = gavToEntityId(gav)
          getAnalysisResultsForEntity(entityId, analysisName, analysisVersion, apiBaseUrl, httpClient).get match {
            case ja: JsArray =>
              val cg = PartialCallGraph(gav, toMethodLookup(ja, gav))
              val types = getAllTypesForProgram(gav, apiBaseUrl, httpClient).get
              theCg.get.addPartialInfo(gav, cg, types)
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

    cg
  }


  def ensureAllPartialResultsPresent(allGavs: Set[String]): Try[Unit] = Try {
    triggerAnalysisRun(allGavs.map(gavToEntityId), analysisName, analysisVersion, apiBaseUrl, httpClient, configuration = "--algorithm rta") match {
      case Success(runLocation) =>
        logger.info(s"Successfully triggered analysis run. Waiting for run at $runLocation to complete...")

        while(!runFinished(runLocation, apiBaseUrl, httpClient).get){
          Thread.sleep(1000)
          logger.debug(s"Waiting for run to finish: $runLocation ...")
        }

        logger.info("All partial callgraphs are available.")
      case Failure(ex) =>
        logger.error("Failed to trigger analysis run for partial callgraphs", ex)
    }
  }

  def close(): Unit = httpClient.close()

  def cleanup(): Unit = {
    theCg = None
  }


  case class PartialCallGraph(gav: String, methodLookup: Map[Long, MethodRepr])

  case class MethodRepr(id: Long, graphGav: String, fqn: String, callSites: List[CallSiteRepr]){

    lazy val identifier: MethodIdentifier = toIdentifier(fqn)

    lazy val incompleteCallSites: Seq[CallSiteRepr] = callSites.filter(_.isIncomplete)

    lazy val hasIncompleteCallSites: Boolean = incompleteCallSites.nonEmpty

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

  class TypeNode(fqn: String, jc: JavaClass, methods: Set[MethodRepr], subNodes: Set[TypeNode] = Set.empty, superNode: Option[TypeNode] = None, interfaces: Set[TypeNode] = Set.empty){

    val typeFqn: String = fqn
    val typeDeclaration: JavaClass = jc
    val allMethods: Set[MethodRepr] = methods

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

  }


  class StitchedCallGraph(rootGav: String, dependencyGavs: Set[String]){

    //TODO: Add RTA aspects

    private val partialCgs = new mutable.HashMap[String, PartialCallGraph]()

    private val rawTypesLookup = new mutable.HashMap[String, JavaClass]()
    private val rawMethodByTypeLookup = new mutable.HashMap[String, mutable.Set[MethodRepr]]

    private var typesLookup = Map.empty[String, TypeNode]

    def addPartialInfo(gav: String, partialCallGraph: PartialCallGraph, types: Seq[JavaClass]): Unit = {
      partialCgs.put(gav, partialCallGraph)
      partialCallGraph.methodLookup.values.foreach(r => {
        val tFqn = r.identifier.mDeclaringTypeFqn
        if(!rawMethodByTypeLookup.contains(tFqn)) rawMethodByTypeLookup.put(tFqn, mutable.Set.empty)
        rawMethodByTypeLookup(tFqn).add(r)
      })
      types.foreach { t => rawTypesLookup.put(t.thisType.replace("/", "."), t) }
    }

    def resolveAll(): Unit = {
      val timeHierarchyOp = timedExec { () =>
        typesLookup = rawTypesLookup.keys.map{ fqn =>
          val methods = rawMethodByTypeLookup.get(fqn).map(_.toSet).getOrElse(Set.empty)
          val classRepr = rawTypesLookup(fqn)
          (fqn, new TypeNode(fqn, classRepr, methods))
        }.toMap
        typesLookup.values.foreach { typeNode =>
          rawTypesLookup(typeNode.typeFqn).superType.flatMap(typesLookup.get(_)).foreach(p => typeNode.setSuperType(p))
          typeNode.setInterfaceTypes(rawTypesLookup(typeNode.typeFqn).interfaceTypes.map(typesLookup(_)))
        }
      }

      logger.info(s"Build type hierarchy in ${timeHierarchyOp.getDurationMillis}ms")



      var start = System.currentTimeMillis()
      val reachableMethods: mutable.Set[MethodRepr] = mutable.Set(partialCgs(rootGav).methodLookup.values.toSeq :_* )

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

          // Get (mutable) Set of targets so far
          val currentTargets = currentCallees(callSite.pc)

          if(callSite.instruction.isStatic){

            // For static callsites we just need to located the declared target type
            val typeFqn = callSite.instruction.declaredTypeFqn
            if(!typesLookup.contains(typeFqn)){
              // This is bad, seem to be missing the type for the static invocation
              logger.warn(s"Unknown type for static invocation: $typeFqn")
            } else {

              // Get the type node and find the one declared method that has a) the same name and b) the same parameters
              val typeNode = typesLookup(typeFqn)
              val targets = typeNode
                .methodLookup
                .getOrElse(callSite.instruction.methodName, Set.empty)
                .filter(m => m.identifier.mArgumentTypes.equals(callSite.instruction.params))

              // There should really only be precisely one matching method for static invocations
              if(targets.isEmpty) logger.warn(s"No matching methods for static invocation ${callSite.instrRep}")
              else if(targets.size > 1) logger.warn(s"Too may matching methods (${targets.size}) for static invocation ${callSite.instrRep}")
              else {
                val theTarget = targets.head

                // Add this new target method to the set
                calleesLookup(currentMethod)(callSite.pc).add(theTarget)

                // Add new target method to set of reachable methods. If it was not present before (i.e. 'new'), add it to worklist as well
                if(reachableMethods.add(theTarget)){
                  toResolve.add(theTarget)
                }

                // Make sure the new target method is contained in the callee lookup
                if(!calleesLookup.contains(theTarget)){
                  val targetGraphMethods = partialCgs(theTarget.graphGav).methodLookup

                  calleesLookup.put(theTarget, theTarget
                    .callSites
                    .map(cs => (cs.pc, mutable.Set(cs.targets.map(tId => targetGraphMethods(tId)) :_*)))
                    .toMap
                  )

                }
              }
            }
          } else {

            val allTypesCHA = {
              val targetTypeFqn = callSite.instruction.declaredTypeFqn
              if(!typesLookup.contains(targetTypeFqn)){
                typesLookup.values.toSet //TODO: Deal with types we don't know (i.e. JRE types)
              } else {
                val targetType = typesLookup(targetTypeFqn)
                targetType.allSubTypes ++ Set(targetType)
              }
            }
            val methods = allTypesCHA.flatMap{t =>t
              .methodLookup
              .getOrElse(callSite.instruction.methodName, Set.empty)
              .filter(m => m.identifier.mArgumentTypes.equals(callSite.instruction.params))
            }

            methods.foreach { m =>
              if(!currentTargets.contains(m)){
                currentTargets.add(m)
              }

              if(!calleesLookup.contains(m)){
                val targetMethods = partialCgs(m.graphGav).methodLookup

                calleesLookup.put(m, m
                  .callSites
                  .map(cs => (cs.pc, mutable.Set(cs.targets.map(tId => targetMethods(tId)) :_*)))
                  .toMap
                )

              }

              if(reachableMethods.add(m)) {
                logger.info(s"Found a new target: ${m.fqn}")
                toResolve.add(m)
              }
            }

            logger.info(s"Got ${methods.size} methods for callsite $callSite")
          }
        }
      }

      val t2 = System.currentTimeMillis() - start
      logger.info(s"Resolver loop took $t2 ms and found ${reachableMethods.size} reachable Methods")

    }






  }
}
