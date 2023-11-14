package org.anon.spareuse.execution.analyses

import org.anon.spareuse.core.model.entities.SoftwareEntityData

trait AnalysisResult {
  val isFresh: Boolean
}

case class ExistingResult(resultUid: String) extends AnalysisResult {
  override val isFresh = false
}
case class FreshResult(content: Object, affectedEntities: Set[SoftwareEntityData]) extends AnalysisResult {
  override val isFresh: Boolean = true
}
