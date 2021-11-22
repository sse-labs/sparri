package org.tud.cgcrawling.maven

import org.slf4j.{Logger, LoggerFactory}

import java.io.{BufferedInputStream, BufferedReader, File, FileInputStream}
import scala.io.Source
import scala.util.Try

class DependencyListProcessor(depFile: File) {

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getDependencies: Try[List[MavenIdentifier]] = Try{
    val source = Source.fromFile(depFile)

    val identifiers = source
      .getLines()
      .filter{ line =>
        !line.trim.toLowerCase.startsWith("finished at:") && !line.trim.toLowerCase.contains("has been relocated")
      }
      .flatMap{ line =>
        val parts = line.trim.split(":")

        if(parts.size > 4){
          log.warn(s"Unexpected identifier format in dependency file: $line")
        }

        if(parts.size >= 3){
          Some(MavenIdentifier(parts(0), parts(1), parts(parts.length - 1)))
        } else {
          log.error(s"Invalid identifier format: $line")
          None
        }
      }
      .toList

    source.close()

    identifiers
  }

}
