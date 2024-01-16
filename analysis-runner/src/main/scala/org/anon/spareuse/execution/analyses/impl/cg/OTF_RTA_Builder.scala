package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaInvocationType, JavaInvokeStatement, JavaMethod, JavaProgram}
import org.opalj.tac.fpcf.properties.cg.InstantiatedTypes
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class OTF_RTA_Builder(programs: Set[JavaProgram], jreVersionToLoad: Option[String]) {

  private final val log: Logger = LoggerFactory.getLogger(getClass)

  private[cg] final val typeLookup: Map[String, TypeNode] = buildTypeHierarchy()
  private[cg] final val classLookup: Map[String, JavaClass] = programs.flatMap(_.allClasses).map(jc => (jc.thisType, jc)).toMap

  private[cg] final val callSiteResolutions: mutable.Map[String, ClassCallSiteResolutions] = mutable.Map.empty

  private[cg] lazy val jreOpt: Option[JreRepresentation] = {

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

  private[cg] lazy val jreClassLookup = jreOpt.map { jreRep =>
    jreRep.types.map { javaType => (javaType.t, javaType) }.toMap
  }.getOrElse(Map.empty)

  private[cg] def objectTypeOpt: Option[TypeNode] = typeLookup.get("java/lang/Object")
  private[cg] def getMethodOnObjectType(jis: JavaInvokeStatement): Option[DefinedMethod] =
    objectTypeOpt.flatMap { objType => findMethodOn(jis, objType) }

  // ------------------------------------------------
  // ------- Defined Methods and their Caches -------
  // ------------------------------------------------

  private val defMCache = mutable.HashMap[JavaMethod, DefinedMethod]()
  def asDefinedMethod(jm: JavaMethod): DefinedMethod = {
    if(!defMCache.contains(jm))
      defMCache(jm) = new DefinedMethod(jm.getEnclosingClass.get.thisType, Some(jm), None)

    defMCache(jm)
  }

  class DefinedMethod(declaringType: String, jmOpt: Option[JavaMethod], jreMethodOpt: Option[JreMethod]){

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

  // -------------------------------------------------------------------
  // ------- Helper Classes for storing intermediate resolutions -------
  // -------------------------------------------------------------------

  class MethodCallSiteResolutions(dm: DefinedMethod){
    private[cg] val callSiteResolutions = new mutable.HashMap[Long, mutable.Set[DefinedMethod]]

    val methodInfo: DefinedMethod = dm

    def makeCallSiteTarget(callSitePc: Long, target: DefinedMethod): Unit = {
      if (callSiteResolutions.contains(callSitePc))
        callSiteResolutions(callSitePc).add(target)
      else
        callSiteResolutions(callSitePc) = mutable.Set(target)
    }

    def getCallSiteResolutions: Seq[(Long, Set[DefinedMethod])] =
      callSiteResolutions.map(t => (t._1, t._2.toSet)).toSeq

    def getResolutions(callSitePc: Long): Option[Set[DefinedMethod]] =
      callSiteResolutions.get(callSitePc).map(_.toSet)
  }

  class ClassCallSiteResolutions(thisTypeFqn: String){
    private[cg] val methodResolutions = new mutable.HashMap[DefinedMethod, MethodCallSiteResolutions]

    val thisType: String = thisTypeFqn

    def makeCallSiteTarget(dm: DefinedMethod, callSitePc: Long, target: DefinedMethod): Unit = {
      if(!methodResolutions.contains(dm))
        methodResolutions(dm) = new MethodCallSiteResolutions(dm)

      methodResolutions(dm).makeCallSiteTarget(callSitePc, target)
    }

  }

  private[cg] final def findMethodOn(jis: JavaInvokeStatement, node: TypeNode): Option[DefinedMethod] = {
    val currentClass = if(node.isJreType) jreClassLookup(node.thisType).asModel else classLookup(node.thisType)

    currentClass.lookupMethod(jis.targetMethodName, jis.targetDescriptor).map(asDefinedMethod)
  }

  @tailrec
  private[cg] final def findMethodDefinition(jis: JavaInvokeStatement, currentNode: TypeNode, recurseParents: Boolean): Option[DefinedMethod] = {

    findMethodOn(jis, currentNode) match {
      case Some(method) => Some(method)
      case None if recurseParents && currentNode.hasParent => findMethodDefinition(jis, currentNode.getParent.get, recurseParents)
      case _ => None
    }

  }

  def getPossibleChildNodes(typeNode: TypeNode, instantiatedTypes: Set[String]): Set[TypeNode] = {
    typeNode.allChildren.filter(cNode => instantiatedTypes.contains(cNode.thisType))
  }


  def resolveInvocation(jis: JavaInvokeStatement, declType: TypeNode, instantiatedTypes: Set[String]): Set[DefinedMethod] = jis.invokeStatementType match {
    case JavaInvocationType.Static =>
      // Static methods only need to be looked for at the precise declared type!
      val targetOpt = findMethodOn(jis, declType)

      if(targetOpt.isEmpty)
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

      if(declTypeM.isDefined) declTypeM.toSet
      else if(!declType.isInterface && declType.hasParent){
        findMethodDefinition(jis, declType, recurseParents = true).toSet
      } else {
        val dmOnObjOpt = getMethodOnObjectType(jis)
        if(declType.isInterface && dmOnObjOpt.isDefined){
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

  def resolveInvocation(jis: JavaInvokeStatement, instantiatedTypes: Set[String]): Set[DefinedMethod] = {

    (jis.targetTypeName.charAt(0):  @scala.annotation.switch) match {
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


  def buildNaive(): Try[Any] = {

    val allInstantiatedTypes = programs
      .flatMap(_.allMethods)
      .flatMap(_.getNewStatements)
      .map(_.instantiatedTypeName) ++ jreOpt.map(_.allTypesInstantiated).getOrElse(Set.empty[String])

    // Create a sequence in which first all program classes will be resolved, then all JRE classes
    val toResolve = programs.flatMap(_.allClasses).toSeq ++ jreOpt.map(_.types.map(_.asModel).toSeq).getOrElse(Seq.empty)

    toResolve
      .foreach{ javaClass =>
        if(!callSiteResolutions.contains(javaClass.thisType))
          callSiteResolutions(javaClass.thisType) = new ClassCallSiteResolutions(javaClass.thisType)

        val ccsr = callSiteResolutions(javaClass.thisType)

        javaClass.getMethods.foreach{ javaMethod =>
          val currentDefinedMethod = asDefinedMethod(javaMethod)
          javaMethod.getInvocationStatements.foreach { jis =>

            resolveInvocation(jis, allInstantiatedTypes)
              .foreach(targetDefinedMethod => ccsr.makeCallSiteTarget(currentDefinedMethod, jis.instructionPc, targetDefinedMethod))
          }
        }
      }

    Success()
  }

  def buildFrom(entryPoint: Any): Try[Any] = ???

  def getGraph(): Any = ???

  private[cg] def buildTypeHierarchy(): Map[String, TypeNode] = {

    val jreTypeMap = if(jreOpt.isDefined){
        jreOpt.get.types.map{ jreType =>
        (jreType.t, buildTypeNode(jreType))
      }.toMap
    } else Map.empty[String, TypeNode]

    val classLookup = programs
      .flatMap(_.allClasses)
      .map(c => (c.thisType, buildTypeNode(c)))
      .toMap ++ jreTypeMap

    classLookup.values.foreach{ tNode =>
      tNode.superTypeOpt.foreach{ s =>
        if(!classLookup.contains(s))
          log.warn(s"Supertype missing: $s")
        else{
          val sNode = classLookup(s)
          tNode.setParent(sNode)
        }
      }

      tNode.interfaceTypes.foreach{ i =>
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


  def buildTypeNode(jc: JavaClass): TypeNode = new TypeNode(jc.thisType, jc.superType, jc.interfaceTypes, jc.isInterface, isJre = false)
  def buildTypeNode(jt: JreType): TypeNode = new TypeNode(jt.t, jt.s, jt.i.toSet, jt.iI, isJre = true)

  private[cg] class TypeNode(thisTypeFqn: String, superTypeFqnOpt: Option[String], interfaceTypeFqns: Set[String], isInterfaceNode: Boolean, isJre: Boolean) {

    private var parent: Option[TypeNode] = None
    private val interfaces: mutable.Set[TypeNode] = mutable.Set.empty
    private val children: mutable.Set[TypeNode] = mutable.Set.empty

    val thisType: String = thisTypeFqn
    val superTypeOpt: Option[String] = superTypeFqnOpt
    val interfaceTypes: Set[String] = interfaceTypeFqns
    val isJreType: Boolean = isJre

    val isInterface: Boolean = isInterfaceNode

    lazy val allChildren: Set[TypeNode] = {
      getChildren.flatMap(c => Set(c) ++ c.allChildren)
    }

    def setParent(p: TypeNode): Unit ={
      parent = Some(p)
      p.children.add(this)
    }
    def hasParent: Boolean = parent.isDefined
    def getParent: Option[TypeNode] = parent

    def addImplements(t: TypeNode): Unit = {
      if(!t.isInterface)
        log.warn(s"${t.thisType} is not an interface type, yet it is implemented by ${thisType}")

      interfaces.add(t)
      t.children.add(this)
    }

    def hasInterfaces: Boolean = interfaces.nonEmpty
    def getInterfaces: Set[TypeNode] = interfaces.toSet

    def getChildren: Set[TypeNode] = children.toSet

    def isIncomplete: Boolean = superTypeOpt.isDefined && parent.isEmpty

  }

}
