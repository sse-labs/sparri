// Copyright (C) 2018 The Delphi Team.
// See the LICENCE file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.tud.cgcrawling.download

import java.util.Locale
import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout
import org.joda.time.format.DateTimeFormat
import org.tud.cgcrawling.AppLogging
import org.tud.cgcrawling.discovery.maven.{JarFile, MavenArtifact, MavenIdentifier, PomFile}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class MavenDownloadActor extends Actor with AppLogging {

  private implicit val shutdownTimeout: Timeout = Timeout(20 seconds)

  override def receive: Receive = {
    case m : MavenIdentifier =>
      implicit val system : ActorSystem = context.system

      val downloader = new HttpDownloader

      val pomResponse = downloader.downloadFromUriWithHeaders(m.toPomLocation.toString)

      pomResponse match {
        case Success((pomStream, pomHeaders)) =>
          log.info(s"Downloaded $m")

          // Extract and parse publication date from header
          val datePattern = DateTimeFormat.forPattern("E, dd MMM yyyy HH:mm:ss zzz").withLocale(Locale.ENGLISH)
          val pomPublicationDate = pomHeaders.find( _.lowercaseName().equals("last-modified") )
            .map( header => Try(datePattern.parseDateTime(header.value())) ) match {
            case Some(Success(date)) => Some(date)
            case Some(Failure(x)) =>
              log.warning(s"Failed to extract publication date for $m: ${x.getMessage}")
              None
            case _ => None
          }

          downloader.downloadFromUri(m.toJarLocation.toString) match {
            case Success(jar) =>
              Await.ready(downloader.httpExt.shutdownAllConnectionPools(), shutdownTimeout.duration)
              sender() ! MavenDownloadActorResponse(
                m,
                Some(MavenArtifact(m, Some(JarFile(jar, m.toJarLocation.toURL)), PomFile(pomStream), pomPublicationDate)),
                dateParsingFailed = pomPublicationDate.isEmpty)
            case Failure(ex) =>
              log.warning(s"Failed to download jar file for $m")
              Await.ready(downloader.httpExt.shutdownAllConnectionPools(), shutdownTimeout.duration)
              sender() ! MavenDownloadActorResponse(
                m,
                Some(MavenArtifact(m, None, PomFile(pomStream), pomPublicationDate)),
                jarDownloadFailed = true,
                dateParsingFailed = pomPublicationDate.isEmpty,
                errorMessage = ex.getMessage
              )
          }

        case Failure(ex) =>
          Await.ready(downloader.httpExt.shutdownAllConnectionPools(), shutdownTimeout.duration)
          log.error(s"Failed to download pom file for $m with message: ${ex.getMessage}")
          sender() ! MavenDownloadActorResponse(m, None, pomDownloadFailed = true, errorMessage = ex.getMessage)
      }

  }
}

case class MavenDownloadActorResponse(identifier: MavenIdentifier,
                                      artifact: Option[MavenArtifact],
                                      pomDownloadFailed: Boolean = false,
                                      jarDownloadFailed: Boolean = false,
                                      dateParsingFailed: Boolean = false,
                                      errorMessage: String = "")

object MavenDownloadActor {
  def props: Props = Props(new MavenDownloadActor)
}


