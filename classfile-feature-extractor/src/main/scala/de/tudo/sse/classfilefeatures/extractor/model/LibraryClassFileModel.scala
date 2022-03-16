package de.tudo.sse.classfilefeatures.extractor.model

import de.tudo.sse.classfilefeatures.common.model.ClassFileRepresentation

import scala.collection.mutable

class LibraryClassFileModel(val classFileThisTypeFqn: String) extends ConditionallyActiveElement[ClassFileRepresentation] {

  val flagsEvolution: ValueEvolution[Int] = new ValueEvolution[Int]
  val majorVersionEvolution: ValueEvolution[Int] = new ValueEvolution[Int]
  val minorVersionEvolution: ValueEvolution[Int] = new ValueEvolution[Int]
  val superTypeEvolution: ValueEvolution[Option[String]] = new ValueEvolution[Option[String]]

  private val interfacesEvolution: ValueEvolution[String] = new ValueEvolution[String]

  private val methodsByIdentifier: mutable.Map[String, LibraryMethodModel] = new mutable.HashMap
  private val fieldDefinitionsByIdentifier: mutable.Map[String, LibraryFieldDefinitionModel] = new mutable.HashMap



  override val identifier: String = classFileThisTypeFqn

  override protected def updateModel(release: String, cfr: ClassFileRepresentation): Unit = {

    flagsEvolution.addValueAt(release, cfr.flags)
    majorVersionEvolution.addValueAt(release, cfr.majorVersion)
    minorVersionEvolution.addValueAt(release, cfr.minorVersion)
    superTypeEvolution.addValueAt(release, cfr.superTypeFqn)

    cfr.interfacesFqn.foreach { interface =>
      interfacesEvolution.addValueAt(release, interface)
    }

    // Update method data
    cfr.methodRepresentations.foreach { mr =>

      // Method identifier incorporates whether method has a body or not. So same signature suddenly having a body will result in two Entries in map
      if(!methodsByIdentifier.contains(mr.identifier)){
        methodsByIdentifier.put(mr.identifier, new LibraryMethodModel(mr))
      }

      methodsByIdentifier(mr.identifier).appendActiveRelease(release, mr)
    }

    // Update field data
    cfr.fieldRepresentations.foreach { fdr =>
      if(!fieldDefinitionsByIdentifier.contains(fdr.identifier)){
        fieldDefinitionsByIdentifier.put(fdr.identifier, new LibraryFieldDefinitionModel(fdr))
      }

      fieldDefinitionsByIdentifier(fdr.identifier).appendActiveRelease(release, fdr)
    }

  }
}
