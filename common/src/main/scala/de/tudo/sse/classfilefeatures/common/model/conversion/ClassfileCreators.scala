package de.tudo.sse.classfilefeatures.common.model.conversion

import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation
import org.opalj.bi.ACC_INTERFACE

trait ClassfileCreator extends FromModelConversion

object ClassfileCreators {

  def buildCreatorWithAiSupport(classFileRepresentations: Seq[ClassFileRepresentation]): ClassfileCreator =
    new OpalClassfileCreator(classFileRepresentations)

  def buildSimpleCreator(classFileRepresentations: Seq[ClassFileRepresentation]): ClassfileCreator =
    new SimpleClassfileCreator(classFileRepresentations)

}

class OpalClassfileCreator(classfileRepresentations: Seq[ClassFileRepresentation]) extends ClassfileCreator {

  private val isInterfaceMap = classfileRepresentations
    .map(cfr => (cfr.thisTypeFqn, (cfr.flags & ACC_INTERFACE.mask) == ACC_INTERFACE.mask))
    .toMap

  override def typeIsInterface(typeFqn: String): Boolean = isInterfaceMap.getOrElse(typeFqn, false)

  override def supportSimpleAi: Boolean = true
}

class SimpleClassfileCreator(classfileRepresentations: Seq[ClassFileRepresentation]) extends ClassfileCreator {

  override def typeIsInterface(typeFqn: String): Boolean = classfileRepresentations
    .find(cfr => cfr.thisTypeFqn.equalsIgnoreCase(typeFqn))
    .exists(cfr => (cfr.flags & ACC_INTERFACE.mask) == ACC_INTERFACE.mask)

  override def supportSimpleAi: Boolean = false
}
