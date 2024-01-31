package org.anon.spareuse.core.model

import org.anon.spareuse.core.formats.AnalysisResultFormat
import org.anon.spareuse.core.model.RunState.RunState
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}

import java.time.LocalDateTime

case class AnalysisData(name: String, version: String, description: String, builtOn: String, registeredBy: String,
                        inputLanguages: Set[String], isRevoked: Boolean, resultFormat: AnalysisResultFormat, inputKind: SoftwareEntityKind,
                        doesBatchProcessing: Boolean, isIncremental: Boolean, executions: Set[AnalysisRunData])

object AnalysisData {
  def systemAnalysis(name: String, version: String, description: String, builtOn: String, languages: Set[String],
                     format: AnalysisResultFormat, inputKind: SoftwareEntityKind, doesBatchProcessing: Boolean, isIncremental: Boolean): AnalysisData = {
    AnalysisData(name, version, description, builtOn, "system", languages, isRevoked = false, format, inputKind, doesBatchProcessing, isIncremental, Set.empty)
  }
}

case class AnalysisRunData(uid: String, timestamp: LocalDateTime, logs: Array[String], configuration: String, state: RunState, isRevoked: Boolean,
                           inputs: Set[SoftwareEntityData], results: Set[AnalysisResultData], parentAnalysisName: String, parentAnalysisVersion: String){
  def withResolvedGenerics(resolver: SoftwareEntityData => SoftwareEntityData, forceResolve: Boolean = false): AnalysisRunData = {

    val resolvedResults = results.map { _.withResolvedGenerics(resolver, forceResolve) }


    AnalysisRunData(
      uid,
      timestamp,
      logs,
      configuration,
      state,
      isRevoked,
      inputs,
      resolvedResults,
      parentAnalysisName,
      parentAnalysisVersion
    )
  }
}

object RunState extends Enumeration {

  type RunState = Value

  val Created: Value = Value(0)
  val Running: Value = Value(1)
  val Failed: Value = Value(2)
  val Finished: Value = Value(3)

}
