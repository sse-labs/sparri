name := "incremental-cg-creator-consumer"

version := "0.1"

scalaVersion := "2.12.14"

enablePlugins(DockerPlugin)

assembly / mainClass := Some("org.tud.cgcrawling.CallGraphCrawler")
assembly / assemblyJarName := "cg-crawler-consumer.jar"

assemblyMergeStrategy := {
  case x: String if x.toLowerCase.contains("manifest.mf") => MergeStrategy.discard
  case x: String if x.toLowerCase.endsWith(".conf") => MergeStrategy.concat
  case x => MergeStrategy.first
}

docker / dockerfile := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("openjdk:8-jre-alpine")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", "-Xms8G", "-Xmx8G", artifactTargetPath)
  }
}

docker / imageNames := Seq(
  // Sets the latest tag
  ImageName(s"${name.value}:latest"),
)

val opalVersion = "4.0.0"
libraryDependencies ++= Seq(
  "de.opal-project" % "common_2.12" % opalVersion,
  "de.opal-project" % "framework_2.12" % opalVersion,
)

val akkaVersion = "2.6.15"
val akkaHttpVersion = "10.1.11"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
)

libraryDependencies += "joda-time" % "joda-time" % "2.10.10"

libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "4.3.1"

libraryDependencies += "org.apache.maven.indexer" % "indexer-reader" % "6.0.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.rabbitmq" % "amqp-client" % "5.13.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
libraryDependencies += "org.scalamock" %% "scalamock" % "5.1.0" % Test