package de.tudo.sse.classfilefeatures.webapi

import de.tudo.sse.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}
import de.tudo.sse.spareuse.core.utils.toHex

package object model {

  def toEntityRepr(entity: SoftwareEntityData): EntityRepr = {
    EntityRepr(entity.name, entity.uid, entity.kind.toString, entity.language, entity.repository, entity.getParent.map(_.uid), entity.binaryHash.map(toHex))
  }

  def genericEntityToEntityRepr(entity: GenericEntityData): EntityRepr = {
    EntityRepr(entity.name, entity. uid, entity.kind.toString, entity.language, entity.repository, entity.parentUid, entity.binaryHash.map(toHex))
  }
}
