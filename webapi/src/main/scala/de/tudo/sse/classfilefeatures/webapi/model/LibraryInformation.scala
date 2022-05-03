package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class LibraryInformation(name: String, versions: Array[String], classes: Array[String])

trait LibraryInformationJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {

  implicit val libInfoFormat: JsonFormat[LibraryInformation] = jsonFormat3(LibraryInformation)
}