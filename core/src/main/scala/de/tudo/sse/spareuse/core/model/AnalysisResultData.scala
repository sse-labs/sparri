package de.tudo.sse.spareuse.core.model

import de.tudo.sse.spareuse.core.formats.AnyValue
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData

class AnalysisResultData(val isRevoked: Boolean, val content: AnyValue[_], affectedEntities: Set[SoftwareEntityData])
