package org.anon.spareuse.core.model

import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}

case class AnalysisResultData(uid: String, isRevoked: Boolean, content: Object, affectedEntities: Set[SoftwareEntityData]){

  def withResolvedGenerics(resolver: String => SoftwareEntityData, forceResolve: Boolean = false): AnalysisResultData = {

    def resolveIfNeeded(sed: SoftwareEntityData) = sed match {
      case g: GenericEntityData =>
        resolver(g.uid)
      case s: SoftwareEntityData if (!s.hasParent && s.getChildren.isEmpty) || forceResolve =>
        resolver(s.uid)
      case s@_ =>
        s
    }

    AnalysisResultData(uid, isRevoked, content, affectedEntities.map(resolveIfNeeded))
  }

}
