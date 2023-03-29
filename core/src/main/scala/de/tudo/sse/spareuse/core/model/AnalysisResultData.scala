package de.tudo.sse.spareuse.core.model

import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData

case class AnalysisResultData(uid: String, isRevoked: Boolean, content: Object, affectedEntities: Set[SoftwareEntityData])
