package org.anon.spareuse.core.maven

import org.scalatest.funspec.AnyFunSpec

class MavenJarDownloaderTest extends AnyFunSpec {

  private val slf4jIdent = MavenIdentifier.fromGAV("org.slf4j:slf4j-api:2.0.0").get
  private val apacheHttpIdent = MavenIdentifier.fromGAV("org.apache.httpcomponents:httpclient:4.5.14").get


  private def assertPublishedAt(ident: MavenIdentifier, date: String): Unit ={
    val downloader = new MavenJarDownloader

    val downloadTry = downloader.downloadJar(ident)

    assert(downloadTry.isSuccess)

    val jar = downloadTry.get

    jar.content.close()

    assert(jar.identifier == ident)
    assert(jar.timeOfUpload == date)

    downloader.shutdown()
  }


  describe("The Maven JAR Downloader"){

    it("must successfully download JARs with their publication date"){
      assertPublishedAt(slf4jIdent, "Sat, 20 Aug 2022 19:08:54 GMT")
      assertPublishedAt(apacheHttpIdent, "Wed, 30 Nov 2022 18:40:53 GMT")
    }

    it("must fail to download invalid identifiers"){
      val downloader = new MavenJarDownloader

      val downloadTry = downloader.downloadJar(MavenIdentifier.fromGAV("foo:bar:1.0.2").get)

      assert(downloadTry.isFailure)

      downloader.shutdown()
    }

  }

}
