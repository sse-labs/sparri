package org.tud.reachablemethods.analysis.dataaccess

import akka.actor.ActorSystem
import akka.http.scaladsl.model.IllegalUriException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must
import org.tud.reachablemethods.analysis.Configuration
import org.tud.reachablemethods.analysis.testutils.withActorSystem

class MethodDataAccessorTest extends AnyFlatSpec with must.Matchers {
  "The ES Accessor" must "error when database host is invalid url" in withActorSystem { system =>
    val accessor = new MethodDataAccessor(buildConfiguration("<<url>>"))(system)

    assertThrows[IllegalUriException](accessor.initialize())
  }

  "The ES Accessor" must "read libraries for correct configurations" in withActorSystem { system =>
    val accessor = new MethodDataAccessor(new Configuration())(system)

    accessor.initialize()

    assert(accessor.libraryInIndex("io.netty:netty-handler"))
  }

  "The ES Accessor" must "read methods for correct configurations" in withActorSystem { system =>
    val accessor = new MethodDataAccessor(new Configuration())(system)

    accessor.initialize()

    val lib = "ch.qos.logback:logback-core"
    val version = "1.0.0"

    assert(accessor.libraryInIndex(lib))

    val libResult = accessor.getArtifactMetadata(lib, version)
    assert(libResult.isSuccess)
    assert(libResult.get.instantiatedTypes.nonEmpty)

    val methods = accessor.getArtifactMethods(lib, version)

    assert(methods.isSuccess && methods.get.nonEmpty)
    assert(methods.get.size == 3274)
  }

  private def buildConfiguration(url: String): Configuration = new Configuration {
    override val elasticClientUri: String = url
  }

}
