package org.anon.spareuse.core.testutils

import org.anon.spareuse.core.model.RunState.RunState
import org.anon.spareuse.core.model.{AnalysisResultData, AnalysisRunData, RunState}
import org.anon.spareuse.core.model.entities.SoftwareEntityData

import java.time.LocalDateTime

object AnalysisTestDataFactory {

  def stringResult(content: String, affects: Set[SoftwareEntityData], uid: String = "NULL", runUid: String = "NULL"): AnalysisResultData = {
    AnalysisResultData(uid, isRevoked = false, runUid, content, affects)
  }

  def analysisRun(analysisName: String, analysisVersion: String, results: Set[AnalysisResultData], inputs: Set[SoftwareEntityData], state: RunState = RunState.Finished, config: String = ""): AnalysisRunData = {
    AnalysisRunData("NULL", LocalDateTime.now(), 60000L, Array.empty, config, state, isRevoked = false, inputs, results, analysisName, analysisVersion)
  }

}
