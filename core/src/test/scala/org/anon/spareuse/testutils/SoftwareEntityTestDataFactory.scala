package org.anon.spareuse.testutils

import org.anon.spareuse.core.model.SoftwareEntityKind
import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind
import org.anon.spareuse.core.model.entities.{GenericEntityData, SoftwareEntityData}

object SoftwareEntityTestDataFactory {

  /**
   * Build a generic entity data object with no hash and no parent. If no arguments are supplied, a default dummy entity
   * of type 'Program' is returned.
   *
   * @param name Name for the generic entity
   * @param uid UID for the generic entity - ensure consistency to name if needed
   * @param kind Kind for the generic entity - defaults to 'Program'
   * @param language Language for the generic entity - defaults to 'Java'
   * @param repository Repository for the generic entity - default to 'Maven'
   * @return GenericEntityData object with no parent
   */
  def genericEntity(name: String = "dummy.group:artifact:1.0.0",
                    uid: String = "dummy.group!dummy.group:artifact:1.0.0",
                    kind: SoftwareEntityKind = SoftwareEntityKind.Program,
                    language: String = "Java",
                    repository: String = "Maven"): SoftwareEntityData =
    new GenericEntityData(name, language, kind, repository, None, uid, None)

}
