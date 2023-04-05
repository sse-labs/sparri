package de.tudo.sse.classfilefeatures.webapi

import de.tudo.sse.spareuse.core.formats.json.CustomFormatWriter
import de.tudo.sse.spareuse.core.model.entities.JavaEntities.{JavaClass, JavaFieldAccessStatement, JavaInvokeStatement, JavaMethod}
import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import de.tudo.sse.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import de.tudo.sse.spareuse.core.utils.toHex

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
    var returnTypeOpt: Option[String] = None
    var paramTypesOpt: Option[Array[String]] = None
    var isInterfaceTypeOpt: Option[Boolean] = None
    var isFinalOpt: Option[Boolean] = None
    var isStaticOpt: Option[Boolean] = None
    var isAbstractOpt: Option[Boolean] = None
    var visibilityOpt: Option[String] = None
    var targetTypeOpt: Option[String] = None

    entity match {
      case jc: JavaClass =>
        thisTypeFqnOpt = Some(jc.thisType)
        superTypeOpt = jc.superType
        interfaceTypesOpt = Some(jc.interfaceTypes.toArray)
        isInterfaceTypeOpt = Some(jc.isInterface)
        isFinalOpt = Some(jc.isFinal)
        isAbstractOpt = Some(jc.isAbstract)
      case jm: JavaMethod =>
        returnTypeOpt = Some(jm.returnType)
        paramTypesOpt = Some(jm.paramTypes.toArray)
        isFinalOpt = Some(jm.isFinal)
        isStaticOpt = Some(jm.isStatic)
        isAbstractOpt = Some(jm.isAbstract)
        visibilityOpt = Some(jm.visibility)
      case jis: JavaInvokeStatement =>
        targetTypeOpt = Some(jis.targetTypeName)
        returnTypeOpt = Some(jis.returnTypeName)
        isStaticOpt = Some(jis.isStaticMethod)
        //TODO: Method Name, Parameter Count, Invocation Types
      case jfas: JavaFieldAccessStatement =>
        targetTypeOpt = Some(jfas.targetTypeName)
        //TODO: Field Type, Field Name, Access Type
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
      returnTypeOpt,
      paramTypesOpt,
      targetTypeOpt
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
      entity.binaryHash.map(toHex), None, None, None, None, None, None, None, None, None, None, None, None)
  }
}
