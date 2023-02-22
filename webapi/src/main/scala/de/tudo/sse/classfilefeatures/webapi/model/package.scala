package de.tudo.sse.classfilefeatures.webapi

import de.tudo.sse.spareuse.core.model.{AnalysisData, AnalysisResultData, AnalysisRunData}
import de.tudo.sse.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import de.tudo.sse.spareuse.core.utils.toHex

import java.time.format.DateTimeFormatter

package object model {


  def toAnalysisRepr(data: AnalysisData): AnalysisInformationRepr = {
    AnalysisInformationRepr(data.name,
      data.description,
      data.version,
      data.isRevoked,
      data.inputKind.toString,
      data.inputLanguages.toArray,
      if(data.builtOn.isBlank) None else Some(data.builtOn),
      data.registeredBy,
      "", //TODO: serialize formats
      data.executions.map(_.uid).toArray)
  }

  def toEntityRepr(entity: SoftwareEntityData): EntityRepr = {
    val children = if(entity.getChildren.isEmpty) None else Some(entity.getChildren.map(toEntityRepr).toArray)
    EntityRepr(entity.name, entity.uid, entity.kind.toString, entity.language, entity.repository, entity.getParent.map(_.uid), entity.binaryHash.map(toHex), children)
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
      data.affectedEntities.map(_.uid) // This only works for fully-built entity trees or generic entities!
    )
  }

  def genericEntityToEntityRepr(entity: GenericEntityData): EntityRepr = {
    EntityRepr(entity.name, entity. uid, entity.kind.toString, entity.language, entity.repository, entity.parentUid, entity.binaryHash.map(toHex), None)
  }
}
