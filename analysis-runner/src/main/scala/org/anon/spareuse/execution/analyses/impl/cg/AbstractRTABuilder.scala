package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaProgram}
import org.anon.spareuse.execution.analyses.impl.cg.AbstractRTABuilder.TypeNode
import org.opalj.br.FieldType
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.switch
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

abstract class AbstractRTABuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) extends CallGraphBuilder {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  // Lookup for type nodes based on their FQN
  protected[cg] final val typeLookup: Map[String, TypeNode] = buildTypeHierarchy()

  // Lookup for Java classes based on their FQN
  protected[cg] override val classLookup: Map[String, JavaClass] =
    programs.flatMap(_.allClasses).map(jc => (jc.thisType, jc)).toMap ++
      jreOpt.map { jreRep => jreRep.types.map { javaType => (javaType.t, javaType.asModel) }.toMap }.getOrElse(Map.empty)

  // The object type node (if JRE is available)
  protected[cg] def objectTypeOpt: Option[TypeNode] = typeLookup.get("java/lang/Object")

  // Tries to find a method definition for the given invocation on the object type
  protected[cg] def getMethodOnObjectType(jis: JavaInvokeStatement): Option[DefinedMethod] =
    objectTypeOpt.flatMap { objType => findMethodOn(jis, objType) }

  // Tries to find a method definition for the given invocation on the given type
  protected[cg] def findMethodOn(jis: JavaInvokeStatement, node: TypeNode): Option[DefinedMethod] = {
    val currentClass = classLookup(node.thisType)

    currentClass.lookupMethod(jis.targetMethodName, jis.targetDescriptor).map(asDefinedMethod)
  }

  // Fully resolved representation of the JRE summaries that shall be used for resolving callgraphs.
  protected[cg] lazy val jreOpt: Option[JreRepresentation] = {

    def tryToOpt: Try[JreRepresentation] => Option[JreRepresentation] = {
      case Success(rep) => Some(rep)
      case Failure(ex) =>
        log.error("Failed to load JRE representation, proceeding without data on JRE types. This may load to useless or wrong results", ex)
        None
    }

    jreVersionToLoad
      .flatMap { version =>
        if (JreModelLoader.hasJre(version)) tryToOpt(JreModelLoader.getJre(version))
        else {
          log.warn(s"Requested JRE version $version to available, using default.")
          tryToOpt(JreModelLoader.getDefaultJre)
        }
      }
  }

  /**
   * Builds the type hierarchy based on all inputs and the JRE version currently specified.
   * @return A Map of type FQNs to their TypeNodes, which hold the parent / child relation
   */
  protected[cg] def buildTypeHierarchy(): Map[String, TypeNode] = {

    val jreTypeMap = if (jreOpt.isDefined) {
      jreOpt.get.types.map { jreType =>
        (jreType.t, buildTypeNode(jreType))
      }.toMap
    } else Map.empty[String, TypeNode]

    val classLookup = programs
      .flatMap(_.allClasses)
      .map(c => (c.thisType, buildTypeNode(c)))
      .toMap ++ jreTypeMap

    classLookup.values.foreach { tNode =>
      tNode.superTypeOpt.foreach { s =>
        if (!classLookup.contains(s))
          log.warn(s"Supertype missing: $s")
        else {
          val sNode = classLookup(s)
          tNode.setParent(sNode)
        }
      }

      tNode.interfaceTypes.foreach { i =>
        if (!classLookup.contains(i))
          log.warn(s"Interface type missing: $i")
        else {
          val iNode = classLookup(i)
          tNode.addImplements(iNode)
        }
      }
    }

    classLookup
  }

  val methodDefCache: mutable.Map[TypeNode, mutable.Map[String, Option[DefinedMethod]]] = mutable.HashMap()

  /**
   * Find the definition of the given method for the given type node. This method will look for the first applicable definition
   * in the parent hierarchy of the given node.
   * @param jis InvocationStatement defining the method that is being looked for
   * @param currentNode The type node to find the matching definition for
   * @param recurseParents Flag to enable looking for matching definitions in parent nodes (needed for virtual invocations)
   * @return Optionally the method definition that matches the invocation statement
   */
  protected[cg] final def findMethodDefinition(jis: JavaInvokeStatement, currentNode: TypeNode, recurseParents: Boolean): Option[DefinedMethod] = {

    val invocationIdent = jis.targetTypeName + jis.targetMethodName + jis.targetDescriptor

    if(recurseParents && methodDefCache.contains(currentNode)){
      if(methodDefCache(currentNode).contains(invocationIdent)) return methodDefCache(currentNode)(invocationIdent)
    }

    findMethodOn(jis, currentNode) match {
      case Some(method) =>
        if(recurseParents){
          if(!methodDefCache.contains(currentNode)) methodDefCache.put(currentNode, mutable.HashMap(invocationIdent -> Some(method)))
          else if(!methodDefCache(currentNode).contains(invocationIdent)) methodDefCache(currentNode)(invocationIdent) = Some(method)
        }
        Some(method)

      case None if recurseParents && currentNode.hasParent =>
        val res = findMethodDefinition(jis, currentNode.getParent.get, recurseParents)
        if (recurseParents) {
          if (!methodDefCache.contains(currentNode)) methodDefCache.put(currentNode, mutable.HashMap(invocationIdent -> res))
          else if (!methodDefCache(currentNode).contains(invocationIdent)) methodDefCache(currentNode)(invocationIdent) = res
        }
        res

      case _ =>
        if(recurseParents){
          if (!methodDefCache.contains(currentNode)) methodDefCache.put(currentNode, mutable.HashMap(invocationIdent -> None))
          else if (!methodDefCache(currentNode).contains(invocationIdent)) methodDefCache(currentNode)(invocationIdent) = None
        }
        None

    }

  }

  /**
   * Retrieve all children of the given type node that are instantiated somewhere
   * @param typeNode TypeNode to enumerate children for
   * @param instantiatedTypes Set of instantiated type names
   * @return Set of TypeNodes that are children of the given node and instantiated somewhere
   */
  protected[cg]def getPossibleChildNodes(typeNode: TypeNode, instantiatedTypes: Set[String]): Set[TypeNode] = {
    typeNode.allChildren.filter(cNode => instantiatedTypes.contains(cNode.thisType))
  }

  private[cg] def resolveOnObject(jis: JavaInvokeStatement, instantiatedTypes: Set[String]): Set[DefinedMethod] =
    instantiatedTypes
      .flatMap(t => classLookup(t).lookupMethod(jis.targetMethodName, jis.targetDescriptor).map(asDefinedMethod)) ++
      getMethodOnObjectType(jis).toSet


  protected[cg] def resolveInvocation(jis: JavaInvokeStatement, declType: TypeNode, instantiatedTypes: Set[String]): Set[DefinedMethod] = jis.invokeStatementType match {
    case JavaInvocationType.Static =>
      // Static methods only need to be looked for at the precise declared type!
      var targetOpt = findMethodOn(jis, declType)

      // If the static call is made inside the same class, it may be a reference to a parent's implementation of the method
      if(targetOpt.isEmpty && jis.getParent.get.asInstanceOf[JavaMethod].enclosingClass.get.thisType == declType.thisType){
        targetOpt = findMethodDefinition(jis, declType, recurseParents = true)
      }

      if (targetOpt.isEmpty)
        log.warn(s"Failed to resolve static invocation on : ${jis.targetTypeName} -> ${jis.targetMethodName}")

      targetOpt.toSet

    case JavaInvocationType.Virtual | JavaInvocationType.Interface =>
      if(declType.thisType == "java/lang/Object") return resolveOnObject(jis, instantiatedTypes)

      val targets = getPossibleChildNodes(declType, instantiatedTypes)
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

  val simpleResolutionCache: mutable.Map[String, Set[DefinedMethod]] = mutable.HashMap()

  /**
   * Resolves the given invocation with the given set of instantiatable types
   * @param jis Invocation statment to resolve
   * @param instantiatedTypes Set of type names that may be instantiated at this point in time
   * @param simpleCaching Set to true in order to cache results only per invocation, not considering types. Only applicable
   *                      if the types considered instantiatable are always the same (ie. in naive RTA).
   * @return Set of defined methods that may be invoked
   */
  def resolveInvocation(jis: JavaInvokeStatement, instantiatedTypes: Set[String], simpleCaching: Boolean = false): Set[DefinedMethod] = {



    if(simpleCaching ){
      val invocationIdent = jis.targetTypeName + jis.targetMethodName + jis.targetDescriptor
      if(simpleResolutionCache.contains(invocationIdent)) return simpleResolutionCache(invocationIdent)
    }

    val result: Set[DefinedMethod] = (jis.targetTypeName.charAt(0): @scala.annotation.switch) match {
      case 'L' =>
        val objTypeFqn = jis.targetTypeName.substring(1, jis.targetTypeName.length - 1)
        if (!typeLookup.contains(objTypeFqn)) {
          log.error(s"No type information available for declared type $objTypeFqn")
          Set.empty
        } else {
          val declaredType = typeLookup(objTypeFqn)
          resolveInvocation(jis, declaredType, instantiatedTypes)
        }
      case '<' =>
        log.warn(s"Resolution for INVOKEDYNAMIC not supported: $jis")
        Set.empty
      case '[' =>
        (jis.targetMethodName: @switch) match {
          case "clone" =>
            // Only performs a memory copy, no other method should be invoked
            Set.empty
          case "hashCode" | "equals" | "toString" =>
            var at = FieldType(jis.targetTypeName)
            while(at.isArrayType) {
              at = at.asArrayType.componentType
            }

            if(at.isObjectType){
              // hashCode, equals and toString on array types involve calling the respective method on their component type!
              // Hence, we rewrite the invoke statement to point to the component type instead
              val correctedInvocation = new JavaInvokeStatement(jis.targetMethodName, at.toJVMTypeName, jis.targetDescriptor, jis.invokeStatementType, jis.instructionPc, jis.uid, jis.repository)
              resolveInvocation(correctedInvocation, instantiatedTypes)
            } else {
              // if one of the three methods is invoked on a primitive component type, we refer to object as the declaring type
              val correctedInvocation = new JavaInvokeStatement(jis.targetMethodName, "java/lang/Object", jis.targetDescriptor, jis.invokeStatementType, jis.instructionPc, jis.uid, jis.repository)
              resolveInvocation(correctedInvocation, instantiatedTypes)
            }
          case _ =>
            log.warn(s"Unable to resolve method invocation on array type: ${jis.targetTypeName}->${jis.targetMethodName}")
            Set.empty
        }

      case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' =>
        log.error(s"Unexpected method invocation on primitive type: $jis")
        Set.empty
      case _ =>
        throw new IllegalArgumentException(s"Not a valid field type: ${jis.targetTypeName}")
    }

    if(simpleCaching){
      val invocationIdent = jis.targetTypeName + jis.targetMethodName + jis.targetDescriptor
      if(!simpleResolutionCache.contains(invocationIdent)) simpleResolutionCache(invocationIdent) = result
    }

    result
  }




  // --------------------------------------------------
  // ------- TypeNode and their Factory Methods -------
  // --------------------------------------------------

  protected def buildTypeNode(jc: JavaClass): TypeNode = new TypeNode(jc.thisType, jc.superType, jc.interfaceTypes, jc.isInterface)

  protected def buildTypeNode(jt: JreType): TypeNode = new TypeNode(jt.t, jt.s, jt.i.toSet, jt.iI)



}

object AbstractRTABuilder {
  class TypeNode(thisTypeFqn: String, superTypeFqnOpt: Option[String], interfaceTypeFqns: Set[String], isInterfaceNode: Boolean) {

    private var parent: Option[TypeNode] = None
    private val interfaces: mutable.Set[TypeNode] = mutable.Set.empty
    private val children: mutable.Set[TypeNode] = mutable.Set.empty

    val thisType: String = thisTypeFqn
    val superTypeOpt: Option[String] = superTypeFqnOpt
    val interfaceTypes: Set[String] = interfaceTypeFqns

    val isInterface: Boolean = isInterfaceNode

    lazy val allChildren: Set[TypeNode] = {
      getChildren.flatMap(c => Set(c) ++ c.allChildren)
    }

    def setParent(p: TypeNode): Unit = {
      parent = Some(p)
      p.children.add(this)
    }

    def hasParent: Boolean = parent.isDefined

    def getParent: Option[TypeNode] = parent

    def addImplements(t: TypeNode): Unit = {
      interfaces.add(t)
      t.children.add(this)
    }

    def hasInterfaces: Boolean = interfaces.nonEmpty

    def getInterfaces: Set[TypeNode] = interfaces.toSet

    def getChildren: Set[TypeNode] = children.toSet

    def isIncomplete: Boolean = superTypeOpt.isDefined && parent.isEmpty

  }
}
