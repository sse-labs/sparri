package de.tudo.sse.classfilefeatures.webapi.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsonFormat}

final case class ReleaseInformation(libraryName: String, releaseName: String, classNames: Array[String])

trait ReleaseInformationJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {

  implicit val releaseInformationFormat: JsonFormat[ReleaseInformation] = jsonFormat3(ReleaseInformation)

}
