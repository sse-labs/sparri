package org.anon.spareuse.core.model.entities

import org.anon.spareuse.core.model.SoftwareEntityKind.SoftwareEntityKind

class GenericEntityData(override val name: String,
                        override val language: String,
                        override val kind: SoftwareEntityKind,
                        override val repository: String,
                        override val binaryHash: Option[Array[Byte]],
                        override val uid: String,
                        val parentUid: Option[String]) extends SoftwareEntityData
