package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaProgram}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

abstract class AbstractRTABuilder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) {

  protected final val log: Logger = LoggerFactory.getLogger(getClass)

  // Lookup for type nodes based on their FQN
  protected[cg] final val typeLookup: Map[String, TypeNode] = buildTypeHierarchy()

  // Lookup for Java classes based on their FQN
  protected[cg] final val classLookup: Map[String, JavaClass] =
    programs.flatMap(_.allClasses).map(jc => (jc.thisType, jc)).toMap ++
      jreOpt.map { jreRep => jreRep.types.map { javaType => (javaType.t, javaType.asModel) }.toMap }.getOrElse(Map.empty)


  protected[cg] val callersOf: mutable.Map[DefinedMethod, mutable.Set[DefinedMethod]] = mutable.Map()
  protected[cg] val calleesOf: mutable.Map[DefinedMethod, mutable.Map[Int, mutable.Set[DefinedMethod]]] = mutable.Map()

  protected[cg] def putCall(from: DefinedMethod, pc: Int, to:DefinedMethod): Unit = {
    if(!calleesOf.contains(from))
      calleesOf(from) = mutable.Map()

    if(!calleesOf(from).contains(pc))
      calleesOf(from)(pc) = mutable.HashSet()

    calleesOf(from)(pc).add(to)

    if(!callersOf.contains(to))
      callersOf(to) = mutable.HashSet()

    callersOf(to).add(from)
  }


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
  private[cg] def buildTypeHierarchy(): Map[String, TypeNode] = {

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

  /**
   * Find the definition of the given method for the given type node. This method will look for the first applicable definition
   * in the parent hierarchy of the given node.
   * @param jis InvocationStatement defining the method that is being looked for
   * @param currentNode The type node to find the matching definition for
   * @param recurseParents Flag to enable looking for matching definitions in parent nodes (needed for virtual invocations)
   * @return Optionally the method definition that matches the invocation statement
   */
  @tailrec
  protected[cg] final def findMethodDefinition(jis: JavaInvokeStatement, currentNode: TypeNode, recurseParents: Boolean): Option[DefinedMethod] = {

    findMethodOn(jis, currentNode) match {
      case Some(method) => Some(method)
      case None if recurseParents && currentNode.hasParent => findMethodDefinition(jis, currentNode.getParent.get, recurseParents)
      case _ => None
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

  protected[cg] def resolveInvocation(jis: JavaInvokeStatement, declType: TypeNode, instantiatedTypes: Set[String]): Set[DefinedMethod] = jis.invokeStatementType match {
    case JavaInvocationType.Static =>
      // Static methods only need to be looked for at the precise declared type!
      val targetOpt = findMethodOn(jis, declType)

      if (targetOpt.isEmpty)
        log.warn(s"Failed to resolve static invocation on : ${jis.targetTypeName}")

      targetOpt.toSet

    case JavaInvocationType.Virtual | JavaInvocationType.Interface =>
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

  /**
   * Resolves the given invocation with the given set of instantiatable types
   * @param jis Invocation statment to resolve
   * @param instantiatedTypes Set of type names that may be instantiated at this point in time
   * @return Set of defined methods that may be invoked
   */
  def resolveInvocation(jis: JavaInvokeStatement, instantiatedTypes: Set[String]): Set[DefinedMethod] = {

    (jis.targetTypeName.charAt(0): @scala.annotation.switch) match {
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
        log.warn(s"No method resolution on array types implemented: ${jis.targetTypeName}->${jis.targetMethodName}")
        Set.empty
      case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' =>
        log.error(s"Unexpected method invocation on primitive type: $jis")
        Set.empty
      case _ =>
        throw new IllegalArgumentException(s"Not a valid field type: ${jis.targetTypeName}")
    }

  }

  // -----------------------------------------------
  // ------- Defined Methods and their Cache -------
  // -----------------------------------------------

  private val defMCache = mutable.HashMap[JavaMethod, DefinedMethod]()

  def asDefinedMethod(jm: JavaMethod): DefinedMethod = {
    if (!defMCache.contains(jm))
      defMCache(jm) = new DefinedMethod(jm.getEnclosingClass.get.thisType, Some(jm), None)

    defMCache(jm)
  }

  class DefinedMethod(declaringType: String, jmOpt: Option[JavaMethod], jreMethodOpt: Option[JreMethod]) {

    val definingTypeName: String = declaringType
    val methodName: String = jmOpt.map(_.name).getOrElse(jreMethodOpt.get.n)
    val descriptor: String = jmOpt.map(_.descriptor).getOrElse(jreMethodOpt.get.d)

    override def equals(obj: Any): Boolean = obj match {
      case other: DefinedMethod =>
        other.definingTypeName.equals(definingTypeName) && other.descriptor.equals(descriptor) && other.methodName.equals(methodName)
      case _ => false
    }

    override def hashCode(): Int = 31 * definingTypeName.hashCode + 11 * methodName.hashCode + 5 * descriptor.hashCode

  }


  // --------------------------------------------------
  // ------- TypeNode and their Factory Methods -------
  // --------------------------------------------------

  protected def buildTypeNode(jc: JavaClass): TypeNode = new TypeNode(jc.thisType, jc.superType, jc.interfaceTypes, jc.isInterface)

  protected def buildTypeNode(jt: JreType): TypeNode = new TypeNode(jt.t, jt.s, jt.i.toSet, jt.iI)

  protected[cg] class TypeNode(thisTypeFqn: String, superTypeFqnOpt: Option[String], interfaceTypeFqns: Set[String], isInterfaceNode: Boolean) {

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
      if (!t.isInterface)
        log.warn(s"${t.thisType} is not an interface type, yet it is implemented by $thisType")

      interfaces.add(t)
      t.children.add(this)
    }

    def hasInterfaces: Boolean = interfaces.nonEmpty

    def getInterfaces: Set[TypeNode] = interfaces.toSet

    def getChildren: Set[TypeNode] = children.toSet

    def isIncomplete: Boolean = superTypeOpt.isDefined && parent.isEmpty

  }

}
