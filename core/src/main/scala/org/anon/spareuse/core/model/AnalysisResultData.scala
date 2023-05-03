package org.anon.spareuse.core.model

import org.anon.spareuse.core.model.entities.SoftwareEntityData

case class AnalysisResultData(uid: String, isRevoked: Boolean, content: Object, affectedEntities: Set[SoftwareEntityData])
