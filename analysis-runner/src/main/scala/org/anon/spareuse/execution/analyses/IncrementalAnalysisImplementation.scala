package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.AnalysisRunData

abstract class IncrementalAnalysisImplementation(protected val baselineRunOpt: Option[AnalysisRunData]) extends AnalysisImplementation {



}
