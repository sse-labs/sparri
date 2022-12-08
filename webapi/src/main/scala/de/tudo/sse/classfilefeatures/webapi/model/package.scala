package de.tudo.sse.classfilefeatures.webapi

import de.tudo.sse.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import de.tudo.sse.spareuse.core.utils.toHex

package object model {

  def toEntityRepr(entity: SoftwareEntityData): EntityRepr = {
    val children = if(entity.getChildren.isEmpty) None else Some(entity.getChildren.map(toEntityRepr).toArray)
    EntityRepr(entity.name, entity.uid, entity.kind.toString, entity.language, entity.repository, entity.getParent.map(_.uid), entity.binaryHash.map(toHex), children)
  }

  def genericEntityToEntityRepr(entity: GenericEntityData): EntityRepr = {
    EntityRepr(entity.name, entity. uid, entity.kind.toString, entity.language, entity.repository, entity.parentUid, entity.binaryHash.map(toHex), None)
  }
}
