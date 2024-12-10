package org.anon.spareuse.core.storage

trait IdentifiableDataBaseEntity {

  private var dbIdOpt: Option[Long] = None

  def hasDataBaseId: Boolean = dbIdOpt.isDefined

  def getDataBaseId: Long = dbIdOpt.get

  def setDataBaseId(id: Long): Unit = dbIdOpt = Some(id)

}
