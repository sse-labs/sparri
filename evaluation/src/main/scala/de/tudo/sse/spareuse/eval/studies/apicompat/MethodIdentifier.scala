package de.tudo.sse.spareuse.eval.studies.apicompat

import de.tudo.sse.spareuse.core.model.entities.JavaEntities.JavaMethod

case class MethodIdentifier(declaredTypeFqn: String, methodName: String, parameterTypes: Seq[String]) {
  override def toString: String = s"$declaredTypeFqn $methodName (${parameterTypes.mkString(",")})"

  override def hashCode(): Int = 13*declaredTypeFqn.hashCode + 11*methodName.hashCode + 5*parameterTypes.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case other: MethodIdentifier => other.declaredTypeFqn.equals(declaredTypeFqn) && other.methodName.equals(methodName) && other.parameterTypes.equals(parameterTypes)
    case _ => false
  }
}

object MethodIdentifier {
  def apply(typeFqn: String, jm: JavaMethod): MethodIdentifier = MethodIdentifier(typeFqn, jm.name, jm.paramTypes)
}
