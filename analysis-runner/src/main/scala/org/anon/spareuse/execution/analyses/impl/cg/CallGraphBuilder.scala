package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaInvokeStatement, JavaMethod}
import org.anon.spareuse.core.storage.IdentifiableDataBaseEntity
import org.anon.spareuse.execution.analyses.impl.cg.CallGraphBuilder.{DefinedMethod, JVMNative, MethodIdent}

import java.util.Objects
import scala.collection.mutable
import scala.util.Try

trait CallGraphBuilder {

  private type ReachableMethodListener = DefinedMethod => Unit

  protected[cg] val callerMap: mutable.Map[DefinedMethod, mutable.Set[DefinedMethod]] = mutable.Map()
  protected[cg] val calleeMap: mutable.Map[DefinedMethod, mutable.Map[Int, mutable.Set[DefinedMethod]]] = mutable.Map()

  protected[cg] def putCall(from: DefinedMethod, pc: Int, to: DefinedMethod): Unit = {
    if (!calleeMap.contains(from)) {
      // If both maps do not contain "from", the method is newly reachable
      if(!callerMap.contains(from)) notifyReachable(from)
      calleeMap(from) = mutable.Map()
    }

    if (!calleeMap(from).contains(pc))
      calleeMap(from)(pc) = mutable.HashSet()

    calleeMap(from)(pc).add(to)

    if (!callerMap.contains(to)) {
      // If both maps to not contain "to", the method is newly reachable
      if(!calleeMap.contains(to)) notifyReachable(to)
      callerMap(to) = mutable.HashSet()
    }

    callerMap(to).add(from)
  }

  protected[cg] def putEntry(method: DefinedMethod): Unit = {
    putCall(JVMNative, -1, method)
  }

  protected[cg] val classLookup: Map[String, JavaClass]


  def buildFrom(dm: DefinedMethod): Try[CallGraphView]

  def buildFrom(jm: JavaMethod): Try[CallGraphView] = buildFrom(asDefinedMethod(jm))

  def getGraph: CallGraphView = new CallGraphView()

  private[cg] var onReachableListenerOpt: Option[ReachableMethodListener] = None

  def setOnReachableMethodListener(listener: ReachableMethodListener): Unit = {
    onReachableListenerOpt = Some(listener)
  }

  private def notifyReachable(dm: DefinedMethod): Unit = onReachableListenerOpt match {
    case Some(listener) if dm != JVMNative => listener(dm)
    case _ =>
  }


  // -----------------------------------------------
  // ------- Defined Methods and their Cache -------
  // -----------------------------------------------

  private val defMCache = mutable.HashMap[JavaMethod, DefinedMethod]()

  def asDefinedMethod(jm: JavaMethod): DefinedMethod = {
    if (!defMCache.contains(jm)) {
      val dm = DefinedMethod(jm.enclosingClass.get.thisType,
        jm.name,
        jm.descriptor,
        jm.isStatic,
        () => jm.newStatements.map(_.instantiatedTypeName).toList,
        () => jm.invocationStatements)

      if(jm.hasDataBaseId) dm.setDataBaseId(jm.getDataBaseId)

      defMCache(jm) = dm
    }

    defMCache(jm)
  }

  class CallGraphView private[cg] extends CallGraph {

    private lazy val methodLookup: Map[MethodIdent, DefinedMethod] = reachableMethods().map(dm => (dm.methodIdentifier, dm)).toMap

    override def reachableMethods(): Set[DefinedMethod] = calleeMap.keySet.toSet ++ callerMap.keySet.toSet

    override def calleesOf(dm: DefinedMethod): Iterable[(Int, Set[DefinedMethod])] = calleeMap.get(dm).map(_.map(t => (t._1, t._2.toSet)).toSeq).getOrElse(Seq.empty)

    override def calleesOf(dm: DefinedMethod, pc: Int): Set[DefinedMethod] = calleeMap.get(dm).map( callSites => callSites.getOrElse(pc, Set.empty[DefinedMethod]).toSet).getOrElse(Set.empty[DefinedMethod])

    override def callersOf(dm: DefinedMethod): Set[DefinedMethod] = callerMap.get(dm).map(_.toSet).getOrElse(Set.empty)

    override protected def lookupMethod(ident: MethodIdent): DefinedMethod = methodLookup(ident)
  }
}

object CallGraphBuilder {

  final class MethodIdent private(val declaredType: String, val methodName: String, val methodDescriptor: String) {

    lazy val sparriMethodIdent: String = JavaEntities.buildMethodIdent(methodName, methodDescriptor)

    override def hashCode(): Int = Objects.hash(declaredType, methodName, methodDescriptor)

    override def equals(obj: Any): Boolean = obj match {
      case other: MethodIdent =>
        other.declaredType == declaredType && other.methodName == methodName && other.methodDescriptor == methodDescriptor
      case _ => false
    }

    override def toString: String = s"$declaredType.$methodName : $methodDescriptor"
  }

  object MethodIdent {

    private final val cache: mutable.Map[String, mutable.Map[String, mutable.Map[String, MethodIdent]]] = mutable.Map.empty

    def apply(declaredType: String, methodName: String, methodDescriptor: String): MethodIdent = {
      if(!cache.contains(declaredType)) cache(declaredType) = mutable.Map.empty
      if(!cache(declaredType).contains(methodName)) cache(declaredType)(methodName) = mutable.Map.empty
      if(!cache(declaredType)(methodName).contains(methodDescriptor))
        cache(declaredType)(methodName)(methodDescriptor) = new MethodIdent(declaredType, methodName, methodDescriptor)

      cache(declaredType)(methodName)(methodDescriptor)
    }
  }

  class DefinedMethod(mIdent: MethodIdent,
                      mIsStatic: Boolean,
                      newTypesProvider: () => List[String],
                      invocationProvider: () => Seq[JavaInvokeStatement]) extends IdentifiableDataBaseEntity {

    val methodIdentifier: MethodIdent = mIdent

    val definingTypeName: String = methodIdentifier.declaredType
    val methodName: String = methodIdentifier.methodName
    val descriptor: String = methodIdentifier.methodDescriptor
    val isStatic: Boolean = mIsStatic

    lazy val newTypesInstantiated: List[String] = newTypesProvider.apply()
    lazy val invocationStatements: Seq[JavaInvokeStatement] = invocationProvider.apply()

    override def equals(obj: Any): Boolean = obj match {
      case other: DefinedMethod =>
        other.methodIdentifier.equals(methodIdentifier)
      case _ => false
    }

    override def hashCode(): Int = methodIdentifier.hashCode()

    override def toString: String = methodIdentifier.toString

  }

  object DefinedMethod {

    def apply(mDeclaredType: String,
              mName: String,
              mDescriptor: String,
              mIsStatic: Boolean,
              mTypesProvider: () => List[String],
              mInvocationProvider: () => Seq[JavaInvokeStatement]): DefinedMethod =
      new DefinedMethod(MethodIdent(mDeclaredType, mName, mDescriptor), mIsStatic, mTypesProvider, mInvocationProvider)

  }

  object JVMNative extends DefinedMethod(MethodIdent("<native>", "<jvm-native>", "<none>"), mIsStatic = true, () => List.empty, () => List.empty)
}
