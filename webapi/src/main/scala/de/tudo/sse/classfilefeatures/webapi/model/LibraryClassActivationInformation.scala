package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

case class LibraryClassActivationInformation(className: String, activeIn: Array[String], alwaysActive: Boolean)

trait LibraryClassActivationInformationJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val libraryClassActivationInfoFormat: JsonFormat[LibraryClassActivationInformation] = jsonFormat3(LibraryClassActivationInformation)
}

case class LibraryClassInformation(className: String,
                                   activeIn: Array[String],
                                   supertypes: Map[Option[String], ConditionallyActiveElementEntry],
                                   flags: Map[String, ConditionallyActiveElementEntry],
                                   methods: Map[String, ConditionallyActiveElementEntry])

case class ConditionallyActiveElementEntry(activeIn: Array[String], alwaysActive: Boolean)

trait LibraryClassInformationJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {

  implicit val conditionallyActiveElementEntryFormat: JsonFormat[ConditionallyActiveElementEntry] = jsonFormat2(ConditionallyActiveElementEntry)


  implicit val libraryClassInfoFormat: JsonFormat[LibraryClassInformation] = jsonFormat5(LibraryClassInformation)
}


