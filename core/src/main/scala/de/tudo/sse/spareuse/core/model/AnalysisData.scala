package de.tudo.sse.spareuse.core.model

import de.tudo.sse.spareuse.core.formats.AnalysisResultFormat
import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import de.tudo.sse.spareuse.core.model.entities.SoftwareEntityData

import java.time.LocalDateTime

case class AnalysisData(name: String, version: String, description: String, builtOn: String, registeredBy: String,
                        inputLanguages: Set[String], isRevoked: Boolean, resultFormat: AnalysisResultFormat, inputKind: SoftwareEntityKind,
                        executions: Set[AnalysisRunData])

case class AnalysisRunData(timestamp: LocalDateTime, logs: Array[String], configuration: String, isRevoked: Boolean,
                           inputs: Set[SoftwareEntityData], results: Set[AnalysisResultData], parentAnalysisName: String, parentAnalysisVersion: String)
