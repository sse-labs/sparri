package de.tudo.sse.spareuse.execution.analyses

import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData

case class Result(content: Object, affectedEntities: Set[SoftwareEntityData])
