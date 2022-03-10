package org.tud.reachablemethods.analysis

import akka.actor.ActorSystem

package object testutils {
  def withActorSystem(implicit consumer: ActorSystem => Unit): Unit ={
    val system = ActorSystem("reachability-analysis-test")
    consumer(system)
    system.terminate()
  }
}
