package org.anon.spareuse.core.model

import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}

case class AnalysisResultData(uid: String, isRevoked: Boolean, originRunUid: String, content: Object, affectedEntities: Set[SoftwareEntityData]){

  def withResolvedGenerics(resolver: SoftwareEntityData => SoftwareEntityData, forceResolve: Boolean = false): AnalysisResultData = {

    def resolveIfNeeded(sed: SoftwareEntityData) = sed match {
      case g: GenericEntityData =>
        resolver(g)
      case s: SoftwareEntityData if (!s.hasParent && !s.isLibrary) || forceResolve =>
        resolver(s)
      case s@_ =>
        s
    }

    AnalysisResultData(uid, isRevoked, originRunUid, content, affectedEntities.map(resolveIfNeeded))
  }

}
