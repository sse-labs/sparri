package de.tudo.sse.classfilefeatures.webapi

object Application {

  def main(args: Array[String]): Unit = {

    val theApp = new ClassfileWebApi()

    if(theApp.initialize()){
      theApp.runUntilButtonPressed()
    } else {
      theApp.shutdown()
    }

  }

}
