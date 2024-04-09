package org.anon.spareuse.webapi

import org.anon.spareuse.core.formats.json.CustomFormatWriter
import org.anon.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod, JavaNewInstanceStatement, JavaProgram}
import org.anon.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import org.anon.spareuse.core.utils.toHex

import java.time.format.DateTimeFormatter

package object model {

  def toAnalysisFormatRepr(data: AnalysisData): AnalysisResultFormatRepr = {
    AnalysisResultFormatRepr(data.resultFormat.formatDescription(), CustomFormatWriter.write(data.resultFormat).compactPrint)
  }

  def toAnalysisRepr(data: AnalysisData): AnalysisInformationRepr = {
    AnalysisInformationRepr(
      data.name,
      data.description,
      data.version,
      data.isRevoked,
      data.inputKind.toString,
      data.inputLanguages.toArray,
      if(data.builtOn.isBlank) None else Some(data.builtOn),
      data.registeredBy,
      CustomFormatWriter.write(data.resultFormat).compactPrint,
      data.executions.map(_.uid).toArray)
  }

  def toEntityRepr(entity: SoftwareEntityData): EntityRepr = {

    var thisTypeFqnOpt: Option[String] = None
    var superTypeOpt: Option[String] = None
    var interfaceTypesOpt: Option[Array[String]] = None
    var descriptorOpt: Option[String] = None
    var isInterfaceTypeOpt: Option[Boolean] = None
    var isFinalOpt: Option[Boolean] = None
    var isStaticOpt: Option[Boolean] = None
    var isAbstractOpt: Option[Boolean] = None
    var visibilityOpt: Option[String] = None
    var targetTypeOpt: Option[String] = None
    var methodHashOpt: Option[Int] = None
    var publicationDateOpt: Option[String] = None

    entity match {
      case jp: JavaProgram =>
        publicationDateOpt = Some(jp.publishedAt)
      case jc: JavaClass =>
        thisTypeFqnOpt = Some(jc.thisType)
        superTypeOpt = jc.superType
        interfaceTypesOpt = Some(jc.interfaceTypes.toArray)
        isInterfaceTypeOpt = Some(jc.isInterface)
        isFinalOpt = Some(jc.isFinal)
        isAbstractOpt = Some(jc.isAbstract)
      case jm: JavaMethod =>
        descriptorOpt = Some(jm.descriptor)
        isFinalOpt = Some(jm.isFinal)
        isStaticOpt = Some(jm.isStatic)
        isAbstractOpt = Some(jm.isAbstract)
        visibilityOpt = Some(jm.visibility)
        methodHashOpt = Some(jm.methodHash)
      case jis: JavaInvokeStatement =>
        targetTypeOpt = Some(jis.targetTypeName)
        descriptorOpt = Some(jis.targetDescriptor)
        isStaticOpt = Some(jis.isStaticMethod)
        //IMPROVE: Method Name, PC, Invocation Types
      case jfas: JavaFieldAccessStatement =>
        targetTypeOpt = Some(jfas.targetTypeName)
        //IMPROVE: Field Type, Field Name, Access Type
      case jnis: JavaNewInstanceStatement =>
        targetTypeOpt = Some(jnis.instantiatedTypeName)
      case _ =>
    }

    val children = if(entity.getChildren.isEmpty) None else Some(entity.getChildren.map(toEntityRepr).toArray)

    EntityRepr(
      entity.name,
      entity.uid,
      entity.kind.toString,
      entity.language,
      entity.repository,
      entity.getParent.map(_.uid),
      entity.binaryHash.map(toHex),
      children,
      thisTypeFqnOpt,
      superTypeOpt,
      interfaceTypesOpt,
      isInterfaceTypeOpt,
      isFinalOpt,
      isStaticOpt,
      isAbstractOpt,
      visibilityOpt,
      descriptorOpt,
      targetTypeOpt,
      methodHashOpt,
      publicationDateOpt
    )
  }

  def toRunRepr(data: AnalysisRunData): AnalysisRunRepr = {
    AnalysisRunRepr(data.uid, data.timestamp.format(DateTimeFormatter.ISO_DATE_TIME), data.logs.toSeq, data.configuration,
      data.state.toString, data.isRevoked, data.parentAnalysisName, data.parentAnalysisVersion, data.inputs.map(toEntityRepr).toSeq)
  }

  def toResultRepr(data: AnalysisResultData): AnalysisResultRepr = {

    if(!data.content.isInstanceOf[String])
      throw new IllegalStateException("Cannot serialize arbitrary objects here")

    AnalysisResultRepr(
      data.uid,
      data.isRevoked,
      data.content.asInstanceOf[String],
      data.affectedEntities.map(_.uid)
    )
  }

  def genericEntityToEntityRepr(entity: GenericEntityData): EntityRepr = {
    EntityRepr(entity.name, entity. uid, entity.kind.toString, entity.language, entity.repository, entity.parentUid,
      entity.binaryHash.map(toHex), None, None, None, None, None, None, None, None, None, None, None, None, None)
  }
}
