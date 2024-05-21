package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaProgram}
import org.anon.spareuse.execution.analyses.impl.cg.AbstractRTABuilder.TypeNode
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.DefinedMethod
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.LookupApplicationMethodRequest
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphResolutionMode.{CHA, NaiveRTA, OracleCallGraphResolutionMode, RTA}

import scala.collection.mutable
import scala.util.Try

class OracleCallGraphBuilder(programs: Set[JavaProgram],
                             applicationTypes: Set[TypeNode],
                             jreVersionToLoad: Option[String], //TODO: Define Obligation Resolver
                             requestApplicationMethodLookup: LookupApplicationMethodRequest => Unit) extends AbstractRTABuilder(programs: Set[JavaProgram],jreVersionToLoad: Option[String]){

  private[cg] final val applicationTypeNames: Set[String] = applicationTypes.map(_.thisType)

  private[cg] final val applicationMethodSummaries: mutable.Map[TypeNode, mutable.Map[String, mutable.Map[String, ApplicationMethod]]] = new mutable.HashMap()

  private[cg] def isApplicationNode(tNode: TypeNode): Boolean =
    applicationTypeNames.contains(tNode.thisType)

  /**
   *     +-------------------------------------------------------------------------------+
   *     | Variables and methods related to selecting a resolution mode                  |
   *     +-------------------------------------------------------------------------------+
   */

  private[cg] var typeNamesInstantiatedInApplication: Set[String] = Set.empty

  private[cg] var resolutionMode: OracleCallGraphResolutionMode = OracleCallGraphResolutionMode.CHA

  def useCHA(): Unit = {
    // If CHA is used, we don't care about types that are instantiatable or not
    resolutionMode = OracleCallGraphResolutionMode.CHA
    typeNamesInstantiatedInApplication = Set.empty
  }

  def useNaiveRTA(typesInstantiatedInApp: Set[String]): Unit = {
    // If naive RTA is used, we need to know what types may be instantiated in the application code
    resolutionMode = OracleCallGraphResolutionMode.NaiveRTA
    typeNamesInstantiatedInApplication = typesInstantiatedInApp
  }

  def useRTA(): Unit = {
    // If real RTA is used, we do not need a global set of instantiatable types, they will be discovered along the way
    resolutionMode = OracleCallGraphResolutionMode.RTA
    typeNamesInstantiatedInApplication = Set.empty
  }

  private[cg] lazy val allInstantiatedLibraryTypes = programs
    .flatMap(_.allMethods)
    .flatMap(_.newStatements)
    .map(_.instantiatedTypeName) ++ jreOpt.map(_.allTypesInstantiated).getOrElse(Set.empty[String])

  private[cg] lazy val allInstantiatedTypes = allInstantiatedLibraryTypes ++ typeNamesInstantiatedInApplication



  override def buildFrom(dm: DefinedMethod): Try[CallGraphView] = buildFrom(dm, Set.empty)


  def buildFrom(dm: DefinedMethod, typesInstantiated: Set[String]): Try[CallGraphView] = {

    resolutionMode match {
      case CHA =>
        resolveNaive(dm, isRTA = false) // No need to pass type information for CHA, only hierarchy is needed
      case NaiveRTA =>
        // Make sure type of current method is instantiatable (if method is not static)
        if (!dm.isStatic && !typeNamesInstantiatedInApplication.contains(dm.definingTypeName))
          typeNamesInstantiatedInApplication = typeNamesInstantiatedInApplication ++ Set(dm.definingTypeName)
        resolveNaive(dm, isRTA = true) // No need to pass type information for naive RTA, algorithm will use global type information
      case RTA =>
        // Make sure type of current method is instantiatable (if method is not static)
        val effectiveTypesInstantiated = if(!dm.isStatic) typesInstantiated ++ Set(dm.definingTypeName) else typesInstantiated
        resolveRTA(dm, effectiveTypesInstantiated) // Pass information on instantiated types
    }
  }

  private[cg] val methodsAnalyzed = mutable.HashSet[Int]()

  private[cg] def resolveNaive(entry: DefinedMethod, isRTA: Boolean): Try[CallGraphView] = Try {
    val workList = mutable.Queue[DefinedMethod](entry)

    while (workList.nonEmpty) {
      val currentMethod = workList.dequeue()

      if (!methodsAnalyzed.contains(currentMethod.hashCode())) {

        currentMethod.invocationStatements.foreach { jis =>
          resolveInvocationNaive(jis, currentMethod, isRTA).foreach { targetDm =>
              putCall(currentMethod, jis.instructionPc, targetDm)
              if (!methodsAnalyzed.contains(targetDm.hashCode()))
                workList.enqueue(targetDm)
          }
        }

        methodsAnalyzed.add(currentMethod.hashCode())
      }
    }

    getGraph
  }

  private[cg] def resolveInvocationNaive(jis: JavaInvokeStatement, context: DefinedMethod, isRTA: Boolean): Set[DefinedMethod] = {

    val typeSelector = (typeName: String) => if(isRTA) allInstantiatedTypes.contains(typeName) else true

    resolveInvocation(jis, context, typeSelector, simpleCaching = true)
  }

  private[cg] def resolveInvocation(jis: JavaInvokeStatement, context: DefinedMethod, typesInstantiated: Set[String]): Set[DefinedMethod] = {
    resolveInvocation(jis, context, typeName => typesInstantiated.contains(typeName))
  }

  protected[cg] override def resolveInvocation(jis: JavaInvokeStatement, declType: TypeNode, callingContext: DefinedMethod, typeSelectable: String => Boolean): Set[DefinedMethod] = {

    val typeExtern = isApplicationNode(declType)

    //TODO: Do something if target type is extern
    def applicationMethodKnown(typeNode: TypeNode, name: String, descriptor: String) = applicationMethodSummaries.contains(typeNode) &&
      applicationMethodSummaries(typeNode).contains(name) && applicationMethodSummaries(typeNode)(name).contains(descriptor)


    jis.invokeStatementType match {
      // If we have a static method invocation on a type that's defined in the application AND we do not have the specific method definition available yet:
      //   ---> Request method definition at client, do not return any potential targets
      case JavaInvocationType.Static if typeExtern && !applicationMethodKnown(declType, jis.targetMethodName, jis.targetDescriptor) =>

        log.info(s"Request lookup of static application method: ${jis.targetMethodName} : ${jis.targetDescriptor}")
        val request = LookupApplicationMethodRequest(jis.targetMethodName, jis.targetDescriptor, Set(declType.thisType), jis.instructionPc, callingContext)
        requestApplicationMethodLookup(request)

        Set.empty

      // If we have a static invocation on a library method OR a application method that we already know --> Resolve it
      case JavaInvocationType.Static =>

        // Static methods only need to be looked for at the precise declared type!
        var targetOpt = findMethodOn(jis, declType)

        // If the static call is made inside the same class, it may be a reference to a parent's implementation of the method
        if (targetOpt.isEmpty && jis.getParent.get.asInstanceOf[JavaMethod].enclosingClass.get.thisType == declType.thisType) {
          targetOpt = findMethodDefinition(jis, declType, recurseParents = true)
        }

        if (targetOpt.isEmpty)
          log.warn(s"Failed to resolve static invocation on : ${jis.targetTypeName} -> ${jis.targetMethodName}")

        targetOpt.toSet

      case JavaInvocationType.Virtual | JavaInvocationType.Interface =>
        if (declType.thisType == "java/lang/Object") return resolveOnObject(jis, typeSelectable)

        val targets = getPossibleChildNodes(declType, typeSelectable)
          .flatMap(node => findMethodOn(jis, node))

        // "Easy" approximation: Always consider base definition if available. Technically it might not be reachable if the
        // declared type is never instantiated and all instantiated subtypes override the method - complex to compute!
        val currentOpt = findMethodDefinition(jis, declType, recurseParents = true)

        targets ++ currentOpt.toSet

      case JavaInvocationType.Special =>

        val declTypeM = findMethodOn(jis, declType)

        if (declTypeM.isDefined) declTypeM.toSet
        else if (!declType.isInterface && declType.hasParent) {
          findMethodDefinition(jis, declType, recurseParents = true).toSet
        } else {
          val dmOnObjOpt = getMethodOnObjectType(jis)
          if (declType.isInterface && dmOnObjOpt.isDefined) {
            dmOnObjOpt.toSet
          } else {
            log.warn(s"Failed to resolve INVOKESPECIAL: No implementation found for ${jis.targetTypeName}->${jis.targetMethodName}")
            Set.empty
          }
        }

      case _ =>
        log.error(s"Unhandled invocation type: ${jis.invokeStatementType}")
        Set.empty
    }
  }





  private[cg] def resolveRTA(entry: DefinedMethod, typesInstantiated: Set[String]): Try[CallGraphView] = { ???
    /*val workStack = mutable.Stack[ResolverTask]()
    val rootSet = mutable.Set.empty[String]


    do {
      workStack.push(ResolverTask(entry, typesInstantiated ++ rootSet.toSet, rootSet))

      while (workStack.nonEmpty) {
        val currTask = workStack.pop()

        val externalTypesPrior = priorInvocations.getOrElse(currTask.method, Set.empty)
        val newExternalTypes = currTask.typesInstantiated.diff(externalTypesPrior)

        if (!priorInvocations.contains(currTask.method) || newExternalTypes.nonEmpty) {

          rootSet.addAll(currTask.method.newTypesInstantiated)

          // What types do we have to look at?
          val effectiveTypesToResolve = currTask.method.newTypesInstantiated ++ newExternalTypes

          currTask.method.invocationStatements.foreach { jis =>
            resolveInvocation(jis, effectiveTypesToResolve).foreach { target =>
              putCall(currTask.method, jis.instructionPc, target)
              workStack.push(ResolverTask(target, effectiveTypesToResolve, rootSet))
            }
          }

          priorInvocations(currTask.method) = externalTypesPrior ++ effectiveTypesToResolve
        }
      }

    } while (rootSet.toSet.diff(priorInvocations(entry)).nonEmpty)


    getGraph*/
  }





  /**
   * Method that builds the composite type hierarchy for a given application and the current set of
   * libraries.
   *
   *  @return A Map of type FQNs to their TypeNodes, which hold the parent / child relation
   */
  override protected[cg] def buildTypeHierarchy(): Map[String, TypeNode] = {
    val allTypesLookup = super.buildTypeHierarchy() ++
      applicationTypes.map(typeNode => (typeNode.thisType, typeNode))

    applicationTypes.foreach { appNode =>
      appNode.superTypeOpt.foreach { s =>
        if (!allTypesLookup.contains(s))
          log.warn(s"Supertype missing: $s")
        else {
          val sNode = allTypesLookup(s)
          appNode.setParent(sNode)
        }
      }

      appNode.interfaceTypes.foreach { i =>
        if (!allTypesLookup.contains(i))
          log.warn(s"Interface type missing: $i")
        else {
          val iNode = allTypesLookup(i)
          appNode.addImplements(iNode)
        }
      }
    }

    allTypesLookup
  }

  private[cg] case class ResolverTask(method: DefinedMethod, typesInstantiated: Set[String], newTypes: mutable.Set[String])

  class ApplicationMethod(declType: String,
                                   mName: String,
                                   mDescriptor: String,
                                   mIsStatic: Boolean,
                                   typesInstantiated: Set[String]) extends DefinedMethod(declType, mName, mDescriptor, mIsStatic, newTypesProvider = () => typesInstantiated, invocationProvider = ???)


}

object OracleCallGraphBuilder {
  case class LookupApplicationMethodRequest(mName: String, mDescriptor: String, types: Set[String], retPC: Int, callingContext: DefinedMethod)

}




