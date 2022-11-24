package de.tudo.sse.classfilefeatures.webapi.storage

import de.tudo.sse.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind

trait WebapiDataAccessor {

  def hasEntity(ident: String, kind: SoftwareEntityKind): Boolean

  def verifyConnectivity(): Unit

  def shutdown(): Unit

}
