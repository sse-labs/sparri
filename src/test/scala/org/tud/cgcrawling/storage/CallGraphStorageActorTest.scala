package org.tud.cgcrawling.storage

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.TestKit
import akka.util.Timeout
import org.neo4j.driver.{Driver, Session, Value}
import org.opalj.tac.cg.XTACallGraphKey
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.tud.cgcrawling.Configuration
import org.tud.cgcrawling.discovery.maven.MavenIdentifier
import org.tud.cgcrawling.download.{MavenDownloadActor, MavenDownloadActorResponse}
import org.tud.cgcrawling.graphgeneration.{CallGraphActor, CallGraphActorResponse, OPALLogAdapter}

import scala.concurrent.Await

class CallGraphStorageActorTest extends TestKit(ActorSystem("Storage-Test")) with AnyFlatSpecLike with must.Matchers with MockFactory{

  private val config = new MyConfig()

  private val downloadActor = system.actorOf(MavenDownloadActor.props)
  private val cgActor = system.actorOf(CallGraphActor.props(config))


  "The Storage Actor" must "process CGs in reasonable time" in {

    val target = MavenIdentifier("https://repo1.maven.org/maven2/", "yom", "yom", "1.0-alpha-2")
    implicit val timeout = Timeout(10 minutes)
    val actor = system.actorOf(CallGraphStorageActor.props(config))

    var startTime: Long = System.currentTimeMillis()
    val cgresponse = buildCG(target)
    val cgDuration = System.currentTimeMillis() - startTime

    println(s"Download and CG generation took $cgDuration ms")

    startTime = System.currentTimeMillis()
    val response = Await.result((actor ? cgresponse).mapTo[CallGraphStorageActorResponse], timeout.duration)
    val storeDuration = System.currentTimeMillis() - startTime

    println(s"Storage procedure took $storeDuration ms")

    assert(response.success)
  }

  private def buildCG(identifier: MavenIdentifier): CallGraphActorResponse = {
    implicit val timeout: Timeout = Timeout(2 minutes)
    val downloadResponse = Await.result((downloadActor ? identifier).mapTo[MavenDownloadActorResponse], timeout.duration)

    assert(!downloadResponse.jarDownloadFailed)
    assert(!downloadResponse.pomDownloadFailed)

    val cgResponse = Await.result((cgActor ? downloadResponse.artifact.get).mapTo[CallGraphActorResponse], timeout.duration)
    assert(cgResponse.success)
    assert(cgResponse.project.isDefined && cgResponse.callgraph.isDefined)

    cgResponse
  }

  private class MyConfig extends Configuration {
    private val neo4jDriverStub = stub[Driver]
    private val neo4jSessionStub = stub[Session]
    (neo4jSessionStub.close _).when().returns().once()
    //(neo4jSessionStub.run(_:String, _:Value)).when(_, _).returns(null)
    (neo4jDriverStub.session: () => Session).when().returns(neo4jSessionStub)

    override val graphDatabaseDriver: Driver = neo4jDriverStub
  }
}
