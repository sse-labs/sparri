package de.tudo.sse.classfilefeatures.webapi.storage.model

case class LibraryClassInformationStorageModel(className: String,
                                               activeIn: Array[String],
                                               supertypes: Map[Option[String], Array[String]],
                                               flags: Map[Int, Array[String]],
                                               methodNames: Map[String, Array[String]])
