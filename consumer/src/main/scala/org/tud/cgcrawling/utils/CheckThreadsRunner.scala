package org.tud.cgcrawling.utils

class CheckThreadsRunner extends Thread {
  override def run(): Unit =  while(true) {
    import scala.collection.JavaConverters._
    val dispThreads =
      Thread.getAllStackTraces.keySet.asScala.filter(_.getName contains "default-dispatcher")

    dispThreads.toVector.map(_.getName).sorted.foreach(println)
    println()
    println(s"Currently ${dispThreads.size} threads")

    Thread.sleep(10000)
  }
}