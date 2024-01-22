package org.anon.spareuse.execution.analyses.impl.cg

import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaMethod}

import scala.collection.mutable
import scala.util.Try

trait CallGraphBuilder {

  protected[cg] val callerMap: mutable.Map[DefinedMethod, mutable.Set[DefinedMethod]] = mutable.Map()
  protected[cg] val calleeMap: mutable.Map[DefinedMethod, mutable.Map[Int, mutable.Set[DefinedMethod]]] = mutable.Map()

  protected[cg] def putCall(from: DefinedMethod, pc: Int, to: DefinedMethod): Unit = {
    if (!calleeMap.contains(from))
      calleeMap(from) = mutable.Map()

    if (!calleeMap(from).contains(pc))
      calleeMap(from)(pc) = mutable.HashSet()

    calleeMap(from)(pc).add(to)

    if (!callerMap.contains(to))
      callerMap(to) = mutable.HashSet()

    callerMap(to).add(from)
  }

  protected[cg] val classLookup: Map[String, JavaClass]


  def buildFrom(dm: DefinedMethod): Try[CallGraphView]

  def buildFrom(jm: JavaMethod): Try[CallGraphView] = buildFrom(asDefinedMethod(jm))

  def getGraph: CallGraphView = new CallGraphView()



  // -----------------------------------------------
  // ------- Defined Methods and their Cache -------
  // -----------------------------------------------

  private val defMCache = mutable.HashMap[JavaMethod, DefinedMethod]()

  def asDefinedMethod(jm: JavaMethod): DefinedMethod = {
    if (!defMCache.contains(jm))
      defMCache(jm) = new DefinedMethod(jm.enclosingClass.get.thisType, Some(jm), None)

    defMCache(jm)
  }

  class DefinedMethod(declaringType: String, jmOpt: Option[JavaMethod], jreMethodOpt: Option[JreMethod]) {

    val definingTypeName: String = declaringType
    val methodName: String = jmOpt.map(_.name).getOrElse(jreMethodOpt.get.n)
    val descriptor: String = jmOpt.map(_.descriptor).getOrElse(jreMethodOpt.get.d)

    lazy val javaMethodOpt: Option[JavaMethod] =
      classLookup.get(definingTypeName).flatMap(_.lookupMethod(methodName, descriptor))

    lazy val newTypesInstantiated: Set[String] =
      javaMethodOpt.map(_.newStatements.map(_.instantiatedTypeName).toSet).getOrElse(Set.empty[String])

    override def equals(obj: Any): Boolean = obj match {
      case other: DefinedMethod =>
        other.definingTypeName.equals(definingTypeName) && other.descriptor.equals(descriptor) && other.methodName.equals(methodName)
      case _ => false
    }

    override def hashCode(): Int = 31 * definingTypeName.hashCode + 11 * methodName.hashCode + 5 * descriptor.hashCode

  }

  class CallGraphView private[cg](){

    def reachableMethods(): Set[DefinedMethod] = calleeMap.keySet.toSet ++ callerMap.keySet.toSet

    def calleesOf(dm: DefinedMethod): Iterable[(Int, Set[DefinedMethod])] = calleeMap.get(dm).map(_.map(t => (t._1, t._2.toSet)).toSeq).getOrElse(Seq.empty)

    def calleesOf(dm: DefinedMethod, pc: Int): Set[DefinedMethod] = calleeMap.get(dm).map( callSites => callSites.getOrElse(pc, throw new IllegalArgumentException(s"Not a callsite: $pc")).toSet).getOrElse(Set.empty[DefinedMethod])

    def callersOf(dm: DefinedMethod): Set[DefinedMethod] = callerMap.get(dm).map(_.toSet).getOrElse(Set.empty)

  }
}
