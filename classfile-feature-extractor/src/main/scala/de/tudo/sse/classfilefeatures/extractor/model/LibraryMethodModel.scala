package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.MethodRepresentation

import scala.collection.mutable

class LibraryMethodModel(prototypeElement: MethodRepresentation) extends ConditionallyActiveElement[MethodRepresentation]{

  val methodName: String = prototypeElement.name
  val jvmMethodDescriptor: String = prototypeElement.jvmMethodDescriptor
  val hasBody: Boolean = prototypeElement.body.isDefined

  val flagsEvolution: ValueEvolution[Int] = new ValueEvolution[Int]
  val maxStackEvolution: ValueEvolution[Int] = new ValueEvolution[Int]
  val maxLocalsEvolution: ValueEvolution[Int] = new ValueEvolution[Int]

  private val invocationsByIdentifier: mutable.Map[String, LibraryInvocationInstructionModel] = new mutable.HashMap
  private val fieldAccessesByIdentifier: mutable.Map[String, LibraryFieldAccessInstructionModel] = new mutable.HashMap

  def invocationEvolutions: Set[LibraryInvocationInstructionModel] = invocationsByIdentifier.values.toSet
  def fieldAccessEvolutions: Set[LibraryFieldAccessInstructionModel] = fieldAccessesByIdentifier.values.toSet

  override val identifier: String = prototypeElement.identifier

  override protected def updateModel(release: String, mr: MethodRepresentation): Unit = {
    flagsEvolution.addValueAt(release, mr.flags)

    if(hasBody){
      maxStackEvolution.addValueAt(release, mr.body.get.maxStack)
      maxLocalsEvolution.addValueAt(release, mr.body.get.maxLocals)

      // Update invocations for this method
      mr.body.get.invocations.foreach { iir =>

        if(!invocationsByIdentifier.contains(iir.identifier)){
          invocationsByIdentifier.put(iir.identifier, new LibraryInvocationInstructionModel(iir))
        }

        invocationsByIdentifier(iir.identifier).appendActiveRelease(release, iir)
      }

      mr.body.get.fieldAccesses.foreach { fair =>

        if(!fieldAccessesByIdentifier.contains(fair.identifier)){
          fieldAccessesByIdentifier.put(fair.identifier, new LibraryFieldAccessInstructionModel(fair))
        }

        fieldAccessesByIdentifier(fair.identifier).appendActiveRelease(release, fair)

      }
    }

  }
}