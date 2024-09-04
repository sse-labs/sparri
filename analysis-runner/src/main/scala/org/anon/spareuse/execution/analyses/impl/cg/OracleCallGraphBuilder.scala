package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaInvocationType, JavaInvokeStatement, JavaProgram}
import org.anon.spareuse.execution.analyses.impl.cg.AbstractRTABuilder.TypeNode
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.DefinedMethod
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphBuilder.{ApplicationMethod, LookupApplicationMethodRequest, LookupApplicationMethodResponse, MethodIdent}
import org.anon.spareuse.execution.analyses.impl.cg.OracleCallGraphResolutionMode.{CHA, NaiveRTA, OracleCallGraphResolutionMode, RTA}

import scala.collection.mutable
import scala.util.Try

class OracleCallGraphBuilder(programs: Set[JavaProgram],
                             applicationTypes: Set[TypeNode],
                             jreVersionToLoad: Option[String],
                             requestApplicationMethodLookup: LookupApplicationMethodRequest => Unit) extends AbstractRTABuilder(programs: Set[JavaProgram],jreVersionToLoad: Option[String]){

  type CallsiteInMethod = (DefinedMethod, Int)

  private[cg] final val applicationTypeNames: Set[String] = applicationTypes.map(_.thisType)

  private[cg] final val libraryEntries: mutable.Set[CallsiteInMethod] = mutable.HashSet.empty

  private[cg] final val applicationMethodSummaries: mutable.Map[TypeNode, mutable.Map[String, mutable.Map[String, Option[ApplicationMethod]]]] = new mutable.HashMap()


  private[cg] def putSummary(node: TypeNode, name: String, descriptor: String, methodOpt: Option[ApplicationMethod]): Unit = {
    if (applicationMethodSummaries.contains(node)) {
      val typeMap = applicationMethodSummaries(node)
      if (typeMap.contains(name)) {
        val nameMap = typeMap(name)
        if (!nameMap.contains(descriptor)) nameMap.put(descriptor, methodOpt)
      } else {
        typeMap.put(name, mutable.HashMap(descriptor -> methodOpt))
      }
    } else {
      applicationMethodSummaries.put(node, mutable.HashMap(name -> mutable.Map(descriptor -> methodOpt)))
    }
  }

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

  def buildFromApplicationMethod(am: ApplicationMethod, typesInstantiated: Set[String], startAtPc: Int = 0): Try[CallGraphView] = {
    putSummary(typeLookup(am.definingTypeName), am.methodName, am.descriptor, Some(am))
    libraryEntries.add((am, startAtPc))
    buildFrom(am, typesInstantiated, startAtPc)
  }
  def buildFrom(dm: DefinedMethod, typesInstantiated: Set[String], startAtPc: Int = 0): Try[CallGraphView] = {

    resolutionMode match {
      case CHA =>
        resolveNaive(dm, isRTA = false, startAtPc) // No need to pass type information for CHA, only hierarchy is needed
      case NaiveRTA =>
        // Make sure type of current method is instantiatable (if method is not static)
        if (!dm.isStatic && !typeNamesInstantiatedInApplication.contains(dm.definingTypeName))
          typeNamesInstantiatedInApplication = typeNamesInstantiatedInApplication ++ Set(dm.definingTypeName)
        resolveNaive(dm, isRTA = true, startAtPc) // No need to pass type information for naive RTA, algorithm will use global type information
      case RTA =>
        // Make sure type of current method is instantiatable (if method is not static)
        val effectiveTypesInstantiated = if(!dm.isStatic) typesInstantiated ++ Set(dm.definingTypeName) else typesInstantiated
        resolveRTA(dm, effectiveTypesInstantiated, startAtPc) // Pass information on instantiated types
    }
  }

  def processResponse(clientResponse: LookupApplicationMethodResponse): Unit = {

    // Register that the requested method is not defined on the noDefTypes returned by the client
    clientResponse.noDefTypes.foreach{ noDefType =>
      putSummary(typeLookup(noDefType), clientResponse.mName, clientResponse.mDescriptor, None)
    }

    // Register that the requested method is defined on the following types
    clientResponse.targetMethods.foreach{ defMethod =>
      putSummary(typeLookup(defMethod.definingTypeName), clientResponse.mName, clientResponse.mDescriptor, Some(defMethod))
    }

    if(resolutionMode == CHA || resolutionMode == NaiveRTA){
      // To resume resolution we want to:
      // - Identify those methods reported by the client that we did not know yet ( => have not analyzed yet)
      // - For those methods put a call from the original calling context to the method in the graph
      // - Restart "normal" resolution at those methods.
      clientResponse
        .targetMethods
        .filterNot(am => methodsAnalyzed.contains(am.hashCode()))
        .foreach { appMethod =>
          findMethod(clientResponse.ccIdent) match {
            case Some(caller) =>
              putCall(caller, clientResponse.ccPC, appMethod)
              resolveNaive(appMethod, resolutionMode == NaiveRTA)
            case None =>
              log.error(s"Client responded with method for which calling context could not be located. Calling Context: ${clientResponse.ccIdent}")
          }

        }
    } else {
      ???
      //TODO: Implement logic for complex RTA
    }

  }

  private[cg] val methodsAnalyzed = mutable.HashSet[Int]()

  private[cg] def resolveNaive(entry: DefinedMethod, isRTA: Boolean, startAtPC: Int = 0): Try[CallGraphView] = Try {
    val workList = mutable.Queue[DefinedMethod](entry)

    var isEntry = true

    while (workList.nonEmpty) {
      val currentMethod = workList.dequeue()

      if (!methodsAnalyzed.contains(currentMethod.hashCode())) {

        currentMethod.invocationStatements.foreach { jis =>

          // Only consider invocations with PC >= startAtPC if this is the entry method
          if(!isEntry || jis.instructionPc >= startAtPC){
            resolveInvocationNaive(jis, currentMethod, isRTA).foreach { targetDm =>
              putCall(currentMethod, jis.instructionPc, targetDm)
              if (!methodsAnalyzed.contains(targetDm.hashCode()))
                workList.enqueue(targetDm)
            }
          }
        }

        methodsAnalyzed.add(currentMethod.hashCode())
      }

      if(isEntry) isEntry = false
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

  protected[cg] override def findMethodOn(jis: JavaInvokeStatement, node: TypeNode): Option[DefinedMethod] = {
    // It is important to override this method so we can handle both application methods that are cached and "regular" methods
    if(isApplicationNode(node)) applicationMethodSummaries.get(node).flatMap(im => im.get(jis.targetMethodName).flatMap(iim => iim.get(jis.targetDescriptor).flatten))
    else super.findMethodOn(jis, node)
  }

  private[cg] def findMethod(ident: MethodIdent): Option[DefinedMethod] = {
    typeLookup.get(ident.declaredType).flatMap { node =>
      if(isApplicationNode(node)) applicationMethodSummaries.get(node).flatMap(im => im.get(ident.methodName).flatMap(iim => iim.get(ident.methodDescriptor).flatten))
      else {
        val currentClass = classLookup(node.thisType)

        currentClass.lookupMethod(ident.methodName, ident.methodDescriptor).map(asDefinedMethod)
      }
    }
  }

  private[cg] def findApplicableDefinition(jis: JavaInvokeStatement, currNode: TypeNode, callingContext: DefinedMethod): Either[Option[DefinedMethod], LookupApplicationMethodRequest] = {

    // If the type itself defines the required method, immediately return it
    val directDefinitionOpt = findMethodOn(jis, currNode)
    if(directDefinitionOpt.isDefined) return Left(directDefinitionOpt)

    val typeExtern = isApplicationNode(currNode)

    val declaredTypeCouldDefineMethod = typeExtern && couldDefineMethod(currNode, jis)

    // If we know that the method does not exist on the defined type OR do not know whether it exists,
    // we look for the one that would currently be valid - and query all types that might have a more specific
    // definition available.

    // This finds the index of the parent that *currently* would have the valid implementation for the method
    val firstDefinitionSiteInParent = currNode
      .allParents
      .zipWithIndex
      .find { parent =>
        // Find the first parent for which we know that is has a matching method definition
        findMethodOn(jis, parent._1).isDefined
      }
      // Return its index
      .map(_._2)

    // The set of types that might have a more recent definition of the required method
    val typesToConsider = if (firstDefinitionSiteInParent.isEmpty) currNode.allParents else currNode.allParents.splitAt(firstDefinitionSiteInParent.get)._1

    // The names of those types that we need to retrieve from the client: Application nodes for which we have not
    // yet queried the current method signature
    var typeNamesToLookup = typesToConsider
      .filter(isApplicationNode)
      .filter(t => couldDefineMethod(t, jis))
      .map(_.thisType)
      .toSet

    // If we do not know if the declared type defines the required method, we need to request the definition at
    // the client, too.
    if (declaredTypeCouldDefineMethod)
      typeNamesToLookup = typeNamesToLookup ++ Set(currNode.thisType)


    if (typeNamesToLookup.nonEmpty) {
      // If there are any types to lookup with the client, we place an appropriate lookup request and return no targets yet
      Right(LookupApplicationMethodRequest(jis.invokeStatementType.id, jis.targetMethodName, jis.targetDescriptor, typeNamesToLookup, jis.instructionPc, MethodIdent(callingContext.definingTypeName, callingContext.methodName, callingContext.descriptor)))
    } else if (firstDefinitionSiteInParent.isEmpty) {
      // We know that no parent defines the method, and there is no type to query that might
      // define it. That means there is no implementation for that method on this type.
      Left(None)
    } else {
      // There are no types to request method definition for, that means the most recent parent it fact has the
      // implementation we are looking for.
      val targetType = currNode.allParents(firstDefinitionSiteInParent.get)
      Left(findMethodOn(jis, targetType))
    }


  }

  private[cg] def couldDefineMethod(typeNode: TypeNode, jis: JavaInvokeStatement): Boolean = !applicationMethodSummaries.contains(typeNode) ||
    !applicationMethodSummaries(typeNode).contains(jis.targetMethodName) || !applicationMethodSummaries(typeNode)(jis.targetMethodName).contains(jis.targetDescriptor)
  protected[cg] override def resolveInvocation(jis: JavaInvokeStatement, declType: TypeNode, callingContext: DefinedMethod, typeSelectable: String => Boolean): Set[DefinedMethod] = {

    val typeExtern = isApplicationNode(declType)
    val staticCallWithinHierarchy = jis.invokeStatementType == JavaInvocationType.Static && callingContext.definingTypeName.equals(declType.thisType)

    def applicationMethodKnown(typeNode: TypeNode): Boolean = applicationMethodSummaries.contains(typeNode) &&
      applicationMethodSummaries(typeNode).contains(jis.targetMethodName) && applicationMethodSummaries(typeNode)(jis.targetMethodName).contains(jis.targetDescriptor)


    val ccIdent = MethodIdent(callingContext.definingTypeName, callingContext.methodName, callingContext.descriptor)


    jis.invokeStatementType match {


      case JavaInvocationType.Static | JavaInvocationType.Special =>

        if(staticCallWithinHierarchy || jis.invokeStatementType == JavaInvocationType.Special){
          // If we have an in-hierarchy static call or a special call:
          //  - We need to locate the implementation of the method. It may be in the current type, or in the parent hierarchy.
          //  - As soon as *one* parent defines the static method, only children of that node could potentially override it.

          val definitionOnDeclaredTypeOpt = findMethodOn(jis, declType)

          if (definitionOnDeclaredTypeOpt.isDefined) {
            // If we know a matching method definition on the declared type, it must be the target
            Set(definitionOnDeclaredTypeOpt.get)
          } else {

            val declaredTypeCouldDefineMethod = typeExtern && !applicationMethodKnown(declType)

            // If we know that the method does not exist on the defined type OR do not know whether it exists,
            // we look for the one that would currently be valid - and query all types that might have a more specific
            // definition available.

            // This finds the index of the parent that *currently* would have the valid implementation for the method
            val firstDefinitionSiteInParent = declType
              .allParents
              .zipWithIndex
              .find { parent =>
                // Find the first parent for which we know that is has a matching method definition
                findMethodOn(jis, parent._1).isDefined
              }
              // Return its index
              .map(_._2)

            // The set of types that might have a more recent definition of the required method
            val typesToConsider = if (firstDefinitionSiteInParent.isEmpty) declType.allParents else declType.allParents.splitAt(firstDefinitionSiteInParent.get)._1

            // The names of those types that we need to retrieve from the client: Application nodes for which we have not
            // yet queried the current method signature
            var typeNamesToLookup = typesToConsider
              .filter(isApplicationNode)
              .filterNot(applicationMethodKnown)
              .map(_.thisType)
              .toSet

            // If we do not know if the declared type defines the required method, we need to request the definition at
            // the client, too.
            if (declaredTypeCouldDefineMethod)
              typeNamesToLookup = typeNamesToLookup ++ Set(declType.thisType)


            if (typeNamesToLookup.nonEmpty) {
              // If there are any types to lookup with the client, we place an appropriate lookup request and return no targets yet
              val request = LookupApplicationMethodRequest(jis.invokeStatementType.id, jis.targetMethodName, jis.targetDescriptor, typeNamesToLookup, jis.instructionPc, ccIdent)
              requestApplicationMethodLookup(request)
              Set.empty
            } else if (firstDefinitionSiteInParent.isEmpty) {
              // We know that no parent defines the method, and there is no type to query that might
              // define it. That means we are missing an implementation for this static call.
              log.error(s"Cannot resolve any targets for static / special call within class hierarchy: $jis")
              Set.empty
            } else {
              // There are no types to request method definition for, that means the most recent parent it fact has the
              // implementation we are looking for.
              val targetType = declType.allParents(firstDefinitionSiteInParent.get)
              Set(findMethodOn(jis, targetType).get)
            }

          }
        } else {
          // If we have a "normal" static call, we either find the target directly, or request its definition iff the
          // the declared type is an application type (and not known yet).
          findMethodOn(jis, declType) match {
            case Some(targetMethod) =>
              Set(targetMethod)
            case None if typeExtern && !applicationMethodKnown(declType) =>
              val request = LookupApplicationMethodRequest(jis.invokeStatementType.id, jis.targetMethodName, jis.targetDescriptor, Set(declType.thisType), jis.instructionPc, ccIdent)
              requestApplicationMethodLookup(request)
              Set.empty
            case None =>
              log.error(s"Cannot resolve any targets for static call: $jis")
              Set.empty
          }
        }

      case JavaInvocationType.Virtual | JavaInvocationType.Interface =>
        // Regardless of whether the declared type is java.lang.Object or any other type - we have to consider all
        // possible children to be valid targets and maybe request their method implementations
        val isObjectTypeCall = declType.thisType == "java/lang/Object"

        val potentialTargets = getPossibleChildNodes(declType, typeSelectable)

        // We need to request all instantiatable children of the declared type, for which we do not know if they define the method
        var needRequesting = potentialTargets
          .filter(isApplicationNode)
          .filterNot(applicationMethodKnown)
          .map(_.thisType)

        // If any potential target defines the method, it becomes an actual target
        var actualTargets = potentialTargets
          .flatMap(n => findMethodOn(jis, n))

        if(isObjectTypeCall){
          // If the call is made on java.lang.Object, we do not need to recurse into any parents - either Object defines
          // the method directly or it is not a valid call.
          actualTargets = actualTargets ++ getMethodOnObjectType(jis).toSet
        } else {
          // If the declared type is anything other than java.lang.Object:
          // Lookup the definition of the desired method on the current type (and its parents)
          findApplicableDefinition(jis, declType, callingContext) match {
            case Left(Some(methodDefinition)) =>
              // If we find a definitive implementation for the declared type, always consider it reachable. Technically it might not be reachable if the
              // declared type is never instantiated and all instantiated subtypes override the method - complex to compute!
              actualTargets = actualTargets ++ Set(methodDefinition)
            case Right(lookupRequest) =>
              // If we find that we need to request more types from the parent hierarchy to find an answer, add them to
              // the set of types that would be requested either way.
              needRequesting = needRequesting ++ lookupRequest.types
            case _ =>
            // If we find out with certainty that there is no implementation on the declared type, we do not have to do anything
          }
        }

        // Request all information missing to fully determine targets
        if(needRequesting.nonEmpty){
          val request = LookupApplicationMethodRequest(jis.invokeStatementType.id, jis.targetMethodName, jis.targetDescriptor, needRequesting, jis.instructionPc, ccIdent)
          requestApplicationMethodLookup(request)
        }

        // Return those targets that we know are valid
        actualTargets

      case _ =>
        log.error(s"Unhandled invocation type: ${jis.invokeStatementType}")
        Set.empty
    }
  }





  private[cg] def resolveRTA(entry: DefinedMethod, typesInstantiated: Set[String], startAtPc: Int): Try[CallGraphView] = { ???
    //TODO: Implement an algorithm that tracks instantiated types here
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




}

object OracleCallGraphBuilder {

  case class MethodIdent(declaredType: String, methodName: String, methodDescriptor: String)

  case class LookupApplicationMethodRequest(mInvokeType: Int, mName: String, mDescriptor: String, types: Set[String], ccPC: Int, ccIdent: MethodIdent)

  class ApplicationMethod(val identifier: MethodIdent,
                          mIsStatic: Boolean,
                          typesInstantiated: List[String],
                          invocations: Seq[JavaInvokeStatement]) extends DefinedMethod(identifier.declaredType, identifier.methodName, identifier.methodDescriptor, mIsStatic, newTypesProvider = () => typesInstantiated, invocationProvider = () => invocations)

  /**
   * Class representing a clients response to a lookup request.
   * @param ccIdent The method identifier of the calling context ( where to resume resolution at )
   * @param ccPC The PC of the calling context
   * @param mName Name of the method that was requested (the call made at ccPC)
   * @param mDescriptor Descriptor of the method that was requested
   * @param typesRequested Set of types that was requested from the client
   * @param targetMethods A set of method definitions that were contributed by the client - application methods
   * @param noDefTypes Set of type names for which no matching definition has been found at the client
   */
  case class LookupApplicationMethodResponse(ccIdent: MethodIdent, ccPC: Int, mName: String, mDescriptor: String, typesRequested: Set[String], targetMethods: Set[ApplicationMethod], noDefTypes: Set[String])

}




