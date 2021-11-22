name := "cg-creator-on-demand-producer"

version := "0.1"

scalaVersion := "2.13.6"

assembly / mainClass := Some("org.tud.cgcrawling.LibraryIdentifierProducer")
assembly / assemblyJarName := "cg-on-demand-producer.jar"

assemblyMergeStrategy := {
  case x: String if x.toLowerCase.contains("manifest.mf") => MergeStrategy.discard
  case x: String if x.toLowerCase.endsWith(".conf") => MergeStrategy.concat
  case x => MergeStrategy.first
}

libraryDependencies += "org.apache.maven.shared" % "maven-invoker" % "3.1.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.6"

libraryDependencies += "com.rabbitmq" % "amqp-client" % "5.13.1"