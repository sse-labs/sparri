package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.entities.SoftwareEntityData

case class Result(content: Object, affectedEntities: Set[SoftwareEntityData])
